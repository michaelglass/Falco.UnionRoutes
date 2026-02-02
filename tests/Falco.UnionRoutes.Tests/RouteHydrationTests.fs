module Falco.UnionRoutes.Tests.RouteHydrationTests

open System
open Xunit
open Swensen.Unquote
open Falco.UnionRoutes
open Microsoft.AspNetCore.Http

// =============================================================================
// Test types
// =============================================================================

type UserId = UserId of Guid
type AdminId = AdminId of Guid

type TestError =
    | NotAuthenticated
    | Forbidden
    | BadRequest of string

// Routes using Pre<'T> for preconditions
type SimpleRoute =
    | List
    | Detail of id: Guid
    | WithAuth of Pre<UserId>
    | WithBoth of Pre<UserId> * id: Guid

type StringRoute = BySlug of slug: string

type IntRoute = ByPage of page: int

type Int64Route = ByBigId of id: int64

type BoolRoute = ByEnabled of enabled: bool

// Wrapper types (single-case DUs)
type PostId = PostId of Guid

type WrapperRoute =
    | ByPostId of id: PostId
    | WithAuthAndPostId of Pre<UserId> * id: PostId

// Custom type for testing custom extractors
type Slug = Slug of string

type CustomTypeRoute =
    | BySlugCustom of slug: Slug
    | WithAuthAndSlug of Pre<UserId> * slug: Slug

// Route without preconditions
type NoAuthRoute =
    | SimpleNoAuth of id: Guid
    | WithWrapper of id: PostId

// Routes for testing Query<'T> (query string parameters)
type QueryRoute =
    | WithQueryString of query: Query<string>
    | WithQueryInt of page: Query<int>
    | WithOptionalQuery of query: Query<string> option
    | WithOptionalQueryInt of page: Query<int> option
    | MixedRouteAndQuery of id: Guid * sort: Query<string>
    | MixedWithOptional of id: Guid * page: Query<int> option

// Route for testing error accumulation
type MultiFieldRoute = TwoRequired of a: Guid * b: int

// Route for testing multiple preconditions
type MultiPreRoute =
    | NeedsUser of Pre<UserId>
    | NeedsAdmin of Pre<AdminId>
    | NeedsBoth of Pre<UserId> * Pre<AdminId>

// =============================================================================
// Mock helpers
// =============================================================================

let createMockContextWithRoute (routeValues: (string * string) list) =
    let context = DefaultHttpContext()

    for (key, value) in routeValues do
        context.Request.RouteValues.Add(key, value)

    context :> HttpContext

let createMockContextWithRouteAndQuery (routeValues: (string * string) list) (queryValues: (string * string) list) =
    let context = DefaultHttpContext()

    for (key, value) in routeValues do
        context.Request.RouteValues.Add(key, value)

    if queryValues.Length > 0 then
        let queryString =
            queryValues
            |> List.map (fun (k, v) -> $"{k}={System.Web.HttpUtility.UrlEncode(v)}")
            |> String.concat "&"

        context.Request.QueryString <- QueryString($"?{queryString}")

    context :> HttpContext

let mockUserAuth: Pipeline<UserId, TestError> =
    fun ctx ->
        match ctx.Request.Headers.TryGetValue("X-User-Id") with
        | true, values ->
            match Guid.TryParse(values.ToString()) with
            | true, guid -> Ok(UserId guid)
            | false, _ -> Error NotAuthenticated
        | false, _ -> Error NotAuthenticated

let mockAdminAuth: Pipeline<AdminId, TestError> =
    fun ctx ->
        match ctx.Request.Headers.TryGetValue("X-Admin-Id") with
        | true, values ->
            match Guid.TryParse(values.ToString()) with
            | true, guid -> Ok(AdminId guid)
            | false, _ -> Error Forbidden
        | false, _ -> Error Forbidden

/// Convert extraction error string to TestError
let makeError (msg: string) = BadRequest msg

/// Combine multiple errors - preserves structure for pattern matching
let combineErrors (errors: TestError list) =
    match errors with
    | [ single ] -> single
    | multiple -> BadRequest(multiple |> List.map string |> String.concat "; ")

// Precondition factories (create fresh each time to avoid module initialization issues)
let userPrecondition () =
    RouteHydration.forPre<UserId, TestError> mockUserAuth

let adminPrecondition () =
    RouteHydration.forPre<AdminId, TestError> mockAdminAuth

let hydrate () =
    RouteHydration.create<SimpleRoute, TestError> [ userPrecondition () ] [] makeError combineErrors

let hydrateString () =
    RouteHydration.create<StringRoute, TestError> [] [] makeError combineErrors

let hydrateInt () =
    RouteHydration.create<IntRoute, TestError> [] [] makeError combineErrors

let hydrateInt64 () =
    RouteHydration.create<Int64Route, TestError> [] [] makeError combineErrors

let hydrateBool () =
    RouteHydration.create<BoolRoute, TestError> [] [] makeError combineErrors

let hydrateWrapper () =
    RouteHydration.create<WrapperRoute, TestError> [ userPrecondition () ] [] makeError combineErrors

// Custom extractor for Slug type
let slugExtractor: TypeExtractor =
    fun fieldName fieldType ctx ->
        if fieldType = typeof<Slug> then
            let route = Falco.Request.getRoute ctx
            let value = route.GetString fieldName

            if String.IsNullOrEmpty(value) then
                Some(Error $"Missing slug: {fieldName}")
            else
                Some(Ok(box (Slug value)))
        else
            None

let hydrateCustom () =
    RouteHydration.create<CustomTypeRoute, TestError> [ userPrecondition () ] [ slugExtractor ] makeError combineErrors

let hydrateNoAuth () =
    RouteHydration.create<NoAuthRoute, TestError> [] [] makeError combineErrors

let hydrateNoAuthWithCustom () =
    RouteHydration.create<CustomTypeRoute, TestError> [] [ slugExtractor ] makeError combineErrors

let hydrateQuery () =
    RouteHydration.create<QueryRoute, TestError> [] [] makeError combineErrors

let hydrateMultiField () =
    RouteHydration.create<MultiFieldRoute, TestError> [] [] makeError combineErrors

let hydrateMultiPre () =
    RouteHydration.create<MultiPreRoute, TestError>
        [ userPrecondition (); adminPrecondition () ]
        []
        makeError
        combineErrors

// =============================================================================
// Unit route tests (no fields)
// =============================================================================

[<Fact>]
let ``hydrates unit route with no changes`` () =
    let ctx = createMockContextWithRoute []
    let pipeline = hydrate () SimpleRoute.List
    test <@ pipeline ctx = Ok SimpleRoute.List @>

// =============================================================================
// Guid field tests
// =============================================================================

[<Fact>]
let ``hydrates Guid field from route params`` () =
    let id = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", id.ToString()) ]
    let pipeline = hydrate () (SimpleRoute.Detail Guid.Empty)
    test <@ pipeline ctx = Ok(SimpleRoute.Detail id) @>

[<Fact>]
let ``returns error for invalid Guid`` () =
    let ctx = createMockContextWithRoute [ ("id", "not-a-guid") ]
    let pipeline = hydrate () (SimpleRoute.Detail Guid.Empty)
    let result = pipeline ctx
    test <@ Result.isError result @>

[<Fact>]
let ``returns error for missing Guid param`` () =
    let ctx = createMockContextWithRoute []
    let pipeline = hydrate () (SimpleRoute.Detail Guid.Empty)
    let result = pipeline ctx
    test <@ Result.isError result @>

[<Fact>]
let ``error message contains field name for missing param`` () =
    let ctx = createMockContextWithRoute []
    let pipeline = hydrate () (SimpleRoute.Detail Guid.Empty)

    match pipeline ctx with
    | Error(BadRequest msg) -> Assert.Contains("id", msg)
    | Error NotAuthenticated -> Assert.Fail("Unexpected NotAuthenticated")
    | Error Forbidden -> Assert.Fail("Unexpected Forbidden")
    | Ok _ -> Assert.Fail("Expected error")

// =============================================================================
// Precondition (Pre<'T>) field tests
// =============================================================================

[<Fact>]
let ``hydrates Pre field from precondition`` () =
    let userId = Guid.NewGuid()
    let ctx = createMockContextWithRoute []
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())
    let pipeline = hydrate () (SimpleRoute.WithAuth(Pre(UserId Guid.Empty)))
    let result = pipeline ctx
    Assert.Equal(Ok(SimpleRoute.WithAuth(Pre(UserId userId))), result)

[<Fact>]
let ``returns precondition error when not authenticated`` () =
    let ctx = createMockContextWithRoute []
    let pipeline = hydrate () (SimpleRoute.WithAuth(Pre(UserId Guid.Empty)))
    let result = pipeline ctx
    // Precondition errors preserve their original type
    Assert.Equal(Error NotAuthenticated, result)

// =============================================================================
// Combined field tests
// =============================================================================

[<Fact>]
let ``hydrates both Pre and Guid fields`` () =
    let userId = Guid.NewGuid()
    let postId = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", postId.ToString()) ]
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())
    let pipeline = hydrate () (SimpleRoute.WithBoth(Pre(UserId Guid.Empty), Guid.Empty))
    let result = pipeline ctx
    Assert.Equal(Ok(SimpleRoute.WithBoth(Pre(UserId userId), postId)), result)

[<Fact>]
let ``returns precondition error even with valid Guid when not authenticated`` () =
    let postId = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", postId.ToString()) ]
    let pipeline = hydrate () (SimpleRoute.WithBoth(Pre(UserId Guid.Empty), Guid.Empty))
    let result = pipeline ctx
    // Precondition errors preserve their original type
    Assert.Equal(Error NotAuthenticated, result)

[<Fact>]
let ``returns Guid error with valid precondition but invalid Guid`` () =
    let userId = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", "invalid") ]
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())
    let pipeline = hydrate () (SimpleRoute.WithBoth(Pre(UserId Guid.Empty), Guid.Empty))
    let result = pipeline ctx
    Assert.True(Result.isError result)

// =============================================================================
// Multiple preconditions tests
// =============================================================================

[<Fact>]
let ``hydrates Pre<UserId> with user precondition`` () =
    let userId = Guid.NewGuid()
    let ctx = createMockContextWithRoute []
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())
    let pipeline = hydrateMultiPre () (MultiPreRoute.NeedsUser(Pre(UserId Guid.Empty)))
    let result = pipeline ctx
    Assert.Equal(Ok(MultiPreRoute.NeedsUser(Pre(UserId userId))), result)

[<Fact>]
let ``hydrates Pre<AdminId> with admin precondition`` () =
    let adminId = Guid.NewGuid()
    let ctx = createMockContextWithRoute []
    ctx.Request.Headers.Append("X-Admin-Id", adminId.ToString())

    let pipeline =
        hydrateMultiPre () (MultiPreRoute.NeedsAdmin(Pre(AdminId Guid.Empty)))

    let result = pipeline ctx
    Assert.Equal(Ok(MultiPreRoute.NeedsAdmin(Pre(AdminId adminId))), result)

[<Fact>]
let ``hydrates both Pre<UserId> and Pre<AdminId>`` () =
    let userId = Guid.NewGuid()
    let adminId = Guid.NewGuid()
    let ctx = createMockContextWithRoute []
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())
    ctx.Request.Headers.Append("X-Admin-Id", adminId.ToString())

    let pipeline =
        hydrateMultiPre () (MultiPreRoute.NeedsBoth(Pre(UserId Guid.Empty), Pre(AdminId Guid.Empty)))

    let result = pipeline ctx
    Assert.Equal(Ok(MultiPreRoute.NeedsBoth(Pre(UserId userId), Pre(AdminId adminId))), result)

[<Fact>]
let ``returns correct error for missing user precondition`` () =
    let ctx = createMockContextWithRoute []
    let pipeline = hydrateMultiPre () (MultiPreRoute.NeedsUser(Pre(UserId Guid.Empty)))
    let result = pipeline ctx
    Assert.Equal(Error NotAuthenticated, result)

[<Fact>]
let ``returns correct error for missing admin precondition`` () =
    let ctx = createMockContextWithRoute []

    let pipeline =
        hydrateMultiPre () (MultiPreRoute.NeedsAdmin(Pre(AdminId Guid.Empty)))

    let result = pipeline ctx
    Assert.Equal(Error Forbidden, result)

[<Fact>]
let ``accumulates errors when both preconditions fail`` () =
    let ctx = createMockContextWithRoute []
    // Neither user nor admin headers set
    let pipeline =
        hydrateMultiPre () (MultiPreRoute.NeedsBoth(Pre(UserId Guid.Empty), Pre(AdminId Guid.Empty)))

    let result = pipeline ctx
    // combineErrors should combine both errors
    match result with
    | Error(BadRequest msg) ->
        // Both NotAuthenticated and Forbidden should be in the combined error
        Assert.Contains("NotAuthenticated", msg)
        Assert.Contains("Forbidden", msg)
    | Error NotAuthenticated -> Assert.Fail("Expected combined error, got single NotAuthenticated")
    | Error Forbidden -> Assert.Fail("Expected combined error, got single Forbidden")
    | Ok _ -> Assert.Fail("Expected error, got Ok")

// =============================================================================
// String field tests
// =============================================================================

[<Fact>]
let ``hydrates string field from route params`` () =
    let ctx = createMockContextWithRoute [ ("slug", "hello-world") ]
    let pipeline = hydrateString () (StringRoute.BySlug "")
    test <@ pipeline ctx = Ok(StringRoute.BySlug "hello-world") @>

[<Fact>]
let ``returns error for empty string param`` () =
    let ctx = createMockContextWithRoute [ ("slug", "") ]
    let pipeline = hydrateString () (StringRoute.BySlug "")
    let result = pipeline ctx
    test <@ Result.isError result @>

[<Fact>]
let ``returns error for missing string param`` () =
    let ctx = createMockContextWithRoute []
    let pipeline = hydrateString () (StringRoute.BySlug "")
    let result = pipeline ctx
    test <@ Result.isError result @>

// =============================================================================
// Int field tests
// =============================================================================

[<Fact>]
let ``hydrates int field from route params`` () =
    let ctx = createMockContextWithRoute [ ("page", "42") ]
    let pipeline = hydrateInt () (IntRoute.ByPage 0)
    test <@ pipeline ctx = Ok(IntRoute.ByPage 42) @>

[<Fact>]
let ``handles negative int`` () =
    let ctx = createMockContextWithRoute [ ("page", "-5") ]
    let pipeline = hydrateInt () (IntRoute.ByPage 0)
    test <@ pipeline ctx = Ok(IntRoute.ByPage -5) @>

[<Fact>]
let ``returns error for non-numeric int param`` () =
    let ctx = createMockContextWithRoute [ ("page", "abc") ]
    let pipeline = hydrateInt () (IntRoute.ByPage 0)
    let result = pipeline ctx
    test <@ Result.isError result @>

[<Fact>]
let ``handles zero int`` () =
    let ctx = createMockContextWithRoute [ ("page", "0") ]
    let pipeline = hydrateInt () (IntRoute.ByPage 1)
    test <@ pipeline ctx = Ok(IntRoute.ByPage 0) @>

[<Fact>]
let ``handles Int32.MaxValue`` () =
    let ctx = createMockContextWithRoute [ ("page", "2147483647") ]
    let pipeline = hydrateInt () (IntRoute.ByPage 0)
    test <@ pipeline ctx = Ok(IntRoute.ByPage 2147483647) @>

[<Fact>]
let ``handles Int32.MinValue`` () =
    let ctx = createMockContextWithRoute [ ("page", "-2147483648") ]
    let pipeline = hydrateInt () (IntRoute.ByPage 0)
    test <@ pipeline ctx = Ok(IntRoute.ByPage -2147483648) @>

[<Fact>]
let ``returns error for int overflow`` () =
    let ctx = createMockContextWithRoute [ ("page", "2147483648") ] // Int32.MaxValue + 1
    let pipeline = hydrateInt () (IntRoute.ByPage 0)
    test <@ Result.isError (pipeline ctx) @>

// =============================================================================
// Int64 field tests
// =============================================================================

[<Fact>]
let ``hydrates int64 field from route params`` () =
    let ctx = createMockContextWithRoute [ ("id", "1234567890") ]
    let pipeline = hydrateInt64 () (Int64Route.ByBigId 0L)
    test <@ pipeline ctx = Ok(Int64Route.ByBigId 1234567890L) @>

[<Fact>]
let ``hydrates negative int64 field`` () =
    // Note: "-9876543210" is outside int32 range, testing true int64 support
    let ctx = createMockContextWithRoute [ ("id", "-9876543210") ]
    let pipeline = hydrateInt64 () (Int64Route.ByBigId 0L)
    test <@ pipeline ctx = Ok(Int64Route.ByBigId -9876543210L) @>

[<Fact>]
let ``hydrates Int64.MaxValue from route params`` () =
    let ctx = createMockContextWithRoute [ ("id", "9223372036854775807") ]
    let pipeline = hydrateInt64 () (Int64Route.ByBigId 0L)
    test <@ pipeline ctx = Ok(Int64Route.ByBigId 9223372036854775807L) @>

[<Fact>]
let ``documents Falco bug - GetString converts large numbers to scientific notation`` () =
    // Documents https://github.com/falcoframework/Falco/issues/149
    // Falco's GetString converts string values through numeric types
    let ctx = createMockContextWithRoute [ ("id", "9223372036854775807") ]

    // Raw RouteValues is correct
    test <@ ctx.Request.RouteValues.["id"].ToString() = "9223372036854775807" @>

    // But Falco's GetString converts it (bug!)
    let route = Falco.Request.getRoute ctx
    test <@ route.GetString "id" = "9.223372036854776E+18" @>

[<Fact>]
let ``documents Falco bug - TryGetInt64 overflows on large values`` () =
    // Documents https://github.com/falcoframework/Falco/issues/149
    let ctx = createMockContextWithRoute [ ("id", "9223372036854775807") ]
    let route = Falco.Request.getRoute ctx

    let threw =
        try
            route.TryGetInt64 "id" |> ignore
            false
        with :? System.OverflowException ->
            true

    test <@ threw @>

[<Fact>]
let ``returns error for non-numeric int64 param`` () =
    let ctx = createMockContextWithRoute [ ("id", "abc") ]
    let pipeline = hydrateInt64 () (Int64Route.ByBigId 0L)
    let result = pipeline ctx
    test <@ Result.isError result @>

// =============================================================================
// Bool field tests
// =============================================================================

[<Fact>]
let ``hydrates bool field from route params - true`` () =
    let ctx = createMockContextWithRoute [ ("enabled", "true") ]
    let pipeline = hydrateBool () (BoolRoute.ByEnabled false)
    test <@ pipeline ctx = Ok(BoolRoute.ByEnabled true) @>

[<Fact>]
let ``hydrates bool field from route params - false`` () =
    let ctx = createMockContextWithRoute [ ("enabled", "false") ]
    let pipeline = hydrateBool () (BoolRoute.ByEnabled true)
    test <@ pipeline ctx = Ok(BoolRoute.ByEnabled false) @>

[<Fact>]
let ``returns error for invalid bool param`` () =
    let ctx = createMockContextWithRoute [ ("enabled", "yes") ]
    let pipeline = hydrateBool () (BoolRoute.ByEnabled false)
    let result = pipeline ctx
    test <@ Result.isError result @>

// =============================================================================
// Wrapper type tests (single-case DUs like PostId)
// =============================================================================

[<Fact>]
let ``hydrates wrapper type field from route params`` () =
    let id = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", id.ToString()) ]
    let pipeline = hydrateWrapper () (WrapperRoute.ByPostId(PostId Guid.Empty))
    let result = pipeline ctx
    Assert.Equal(Ok(WrapperRoute.ByPostId(PostId id)), result)

[<Fact>]
let ``returns error for invalid Guid in wrapper type`` () =
    let ctx = createMockContextWithRoute [ ("id", "not-a-guid") ]
    let pipeline = hydrateWrapper () (WrapperRoute.ByPostId(PostId Guid.Empty))
    let result = pipeline ctx
    Assert.True(Result.isError result)

[<Fact>]
let ``hydrates both Pre and wrapper type fields`` () =
    let userId = Guid.NewGuid()
    let postId = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", postId.ToString()) ]
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())

    let pipeline =
        hydrateWrapper () (WrapperRoute.WithAuthAndPostId(Pre(UserId Guid.Empty), PostId Guid.Empty))

    let result = pipeline ctx
    Assert.Equal(Ok(WrapperRoute.WithAuthAndPostId(Pre(UserId userId), PostId postId)), result)

// =============================================================================
// Custom extractor tests
// =============================================================================

[<Fact>]
let ``custom extractor handles custom type`` () =
    let ctx = createMockContextWithRoute [ ("slug", "hello-world") ]
    let pipeline = hydrateNoAuthWithCustom () (CustomTypeRoute.BySlugCustom(Slug ""))
    test <@ pipeline ctx = Ok(CustomTypeRoute.BySlugCustom(Slug "hello-world")) @>

[<Fact>]
let ``custom extractor returns error for missing value`` () =
    let ctx = createMockContextWithRoute []
    let pipeline = hydrateNoAuthWithCustom () (CustomTypeRoute.BySlugCustom(Slug ""))
    let result = pipeline ctx
    test <@ Result.isError result @>

[<Fact>]
let ``custom extractor works with Pre`` () =
    let userId = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("slug", "my-post") ]
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())

    let pipeline =
        hydrateCustom () (CustomTypeRoute.WithAuthAndSlug(Pre(UserId Guid.Empty), Slug ""))

    let result = pipeline ctx
    Assert.Equal(Ok(CustomTypeRoute.WithAuthAndSlug(Pre(UserId userId), Slug "my-post")), result)

// =============================================================================
// No precondition tests
// =============================================================================

[<Fact>]
let ``hydrates Guid field without preconditions`` () =
    let id = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", id.ToString()) ]
    let pipeline = hydrateNoAuth () (NoAuthRoute.SimpleNoAuth Guid.Empty)
    test <@ pipeline ctx = Ok(NoAuthRoute.SimpleNoAuth id) @>

[<Fact>]
let ``hydrates wrapper type without preconditions`` () =
    let id = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", id.ToString()) ]
    let pipeline = hydrateNoAuth () (NoAuthRoute.WithWrapper(PostId Guid.Empty))
    test <@ pipeline ctx = Ok(NoAuthRoute.WithWrapper(PostId id)) @>

// =============================================================================
// Query parameter tests
// =============================================================================

[<Fact>]
let ``Query string field extracts from query`` () =
    let ctx = createMockContextWithRouteAndQuery [] [ ("query", "hello") ]
    let pipeline = hydrateQuery () (QueryRoute.WithQueryString(Query ""))
    test <@ pipeline ctx = Ok(QueryRoute.WithQueryString(Query "hello")) @>

[<Fact>]
let ``Query int field extracts from query`` () =
    let ctx = createMockContextWithRouteAndQuery [] [ ("page", "10") ]
    let pipeline = hydrateQuery () (QueryRoute.WithQueryInt(Query 0))
    test <@ pipeline ctx = Ok(QueryRoute.WithQueryInt(Query 10)) @>

[<Fact>]
let ``Query field returns error when missing`` () =
    let ctx = createMockContextWithRouteAndQuery [] []
    let pipeline = hydrateQuery () (QueryRoute.WithQueryString(Query ""))
    test <@ Result.isError (pipeline ctx) @>

[<Fact>]
let ``optional Query field returns Some when present`` () =
    let ctx = createMockContextWithRouteAndQuery [] [ ("query", "search") ]
    let pipeline = hydrateQuery () (QueryRoute.WithOptionalQuery None)
    test <@ pipeline ctx = Ok(QueryRoute.WithOptionalQuery(Some(Query "search"))) @>

[<Fact>]
let ``optional Query field returns None when missing`` () =
    let ctx = createMockContextWithRouteAndQuery [] []
    let pipeline = hydrateQuery () (QueryRoute.WithOptionalQuery None)
    test <@ pipeline ctx = Ok(QueryRoute.WithOptionalQuery None) @>

[<Fact>]
let ``mixed route and query parameters`` () =
    let id = Guid.NewGuid()

    let ctx =
        createMockContextWithRouteAndQuery [ ("id", id.ToString()) ] [ ("sort", "date") ]

    let pipeline = hydrateQuery () (QueryRoute.MixedRouteAndQuery(Guid.Empty, Query ""))
    test <@ pipeline ctx = Ok(QueryRoute.MixedRouteAndQuery(id, Query "date")) @>

[<Fact>]
let ``optional Query int returns Some when present`` () =
    let ctx = createMockContextWithRouteAndQuery [] [ ("page", "42") ]
    let pipeline = hydrateQuery () (QueryRoute.WithOptionalQueryInt None)
    test <@ pipeline ctx = Ok(QueryRoute.WithOptionalQueryInt(Some(Query 42))) @>

[<Fact>]
let ``optional Query int returns None when missing`` () =
    let ctx = createMockContextWithRouteAndQuery [] []
    let pipeline = hydrateQuery () (QueryRoute.WithOptionalQueryInt None)
    test <@ pipeline ctx = Ok(QueryRoute.WithOptionalQueryInt None) @>

[<Fact>]
let ``optional Query int returns error for invalid value`` () =
    let ctx = createMockContextWithRouteAndQuery [] [ ("page", "abc") ]
    let pipeline = hydrateQuery () (QueryRoute.WithOptionalQueryInt None)
    test <@ Result.isError (pipeline ctx) @>

[<Fact>]
let ``mixed route param and optional query`` () =
    let id = Guid.NewGuid()

    let ctx =
        createMockContextWithRouteAndQuery [ ("id", id.ToString()) ] [ ("page", "5") ]

    let pipeline = hydrateQuery () (QueryRoute.MixedWithOptional(Guid.Empty, None))
    test <@ pipeline ctx = Ok(QueryRoute.MixedWithOptional(id, Some(Query 5))) @>

[<Fact>]
let ``mixed route param works without optional query`` () =
    let id = Guid.NewGuid()
    let ctx = createMockContextWithRouteAndQuery [ ("id", id.ToString()) ] []
    let pipeline = hydrateQuery () (QueryRoute.MixedWithOptional(Guid.Empty, None))
    test <@ pipeline ctx = Ok(QueryRoute.MixedWithOptional(id, None)) @>

// =============================================================================
// Error accumulation tests
// =============================================================================

[<Fact>]
let ``hydration collects all errors`` () =
    let ctx = createMockContextWithRoute []
    let pipeline = hydrateMultiField () (MultiFieldRoute.TwoRequired(Guid.Empty, 0))
    let result = pipeline ctx

    match result with
    | Error(BadRequest msg) -> test <@ msg.Contains("a") && msg.Contains("b") @>
    | Error NotAuthenticated -> Assert.Fail("Unexpected NotAuthenticated error")
    | Error Forbidden -> Assert.Fail("Unexpected Forbidden error")
    | Ok route -> Assert.Fail($"Expected error but got: {route}")

[<Fact>]
let ``hydration succeeds when all fields valid`` () =
    let id = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("a", id.ToString()); ("b", "42") ]
    let pipeline = hydrateMultiField () (MultiFieldRoute.TwoRequired(Guid.Empty, 0))
    test <@ pipeline ctx = Ok(MultiFieldRoute.TwoRequired(id, 42)) @>

// =============================================================================
// OptPre<'T> tests (skippable preconditions)
// =============================================================================

// Routes using OptPre<'T> for skippable preconditions
type OptPreRoute =
    | WithOptAuth of OptPre<UserId>
    | WithBothPreTypes of Pre<AdminId> * OptPre<UserId>
    | WithOptAuthAndId of OptPre<UserId> * id: Guid

// Nested routes demonstrating skip behavior
type ChildRoute =
    | Normal
    | [<SkipAllPreconditions>] Public
    | [<SkipPrecondition(typeof<UserId>)>] PartiallyPublic

type ParentWithOptPre = Children of OptPre<UserId> * ChildRoute

type ParentWithBothPre = Children of Pre<AdminId> * OptPre<UserId> * ChildRoute

let optPrePrecondition () =
    RouteHydration.forOptPre<UserId, TestError> mockUserAuth

let hydrateOptPre () =
    RouteHydration.create<OptPreRoute, TestError> [ optPrePrecondition () ] [] makeError combineErrors

let hydrateOptPreWithAdmin () =
    RouteHydration.create<OptPreRoute, TestError>
        [ optPrePrecondition (); adminPrecondition () ]
        []
        makeError
        combineErrors

let hydrateParentWithOptPre () =
    RouteHydration.create<ParentWithOptPre, TestError> [ optPrePrecondition () ] [] makeError combineErrors

let hydrateParentWithBothPre () =
    RouteHydration.create<ParentWithBothPre, TestError>
        [ adminPrecondition (); optPrePrecondition () ]
        []
        makeError
        combineErrors

[<Fact>]
let ``OptPre field hydrates from precondition when authenticated`` () =
    let userId = Guid.NewGuid()
    let ctx = createMockContextWithRoute []
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())
    let pipeline = hydrateOptPre () (OptPreRoute.WithOptAuth(OptPre(UserId Guid.Empty)))
    let result = pipeline ctx
    Assert.Equal(Ok(OptPreRoute.WithOptAuth(OptPre(UserId userId))), result)

[<Fact>]
let ``OptPre field returns error when not authenticated (without skip)`` () =
    let ctx = createMockContextWithRoute []
    let pipeline = hydrateOptPre () (OptPreRoute.WithOptAuth(OptPre(UserId Guid.Empty)))
    let result = pipeline ctx
    Assert.Equal(Error NotAuthenticated, result)

[<Fact>]
let ``OptPre with SkipAllPreconditions provides sentinel value`` () =
    let ctx = createMockContextWithRoute []
    // No auth header - would normally fail
    let pipeline =
        hydrateParentWithOptPre () (ParentWithOptPre.Children(OptPre(UserId Guid.Empty), ChildRoute.Public))

    let result = pipeline ctx
    // Should succeed because OptPre is skipped - provides sentinel value
    match result with
    | Ok(ParentWithOptPre.Children(OptPre(UserId uid), ChildRoute.Public)) ->
        // Sentinel value should be Guid.Empty (default for value type inside wrapper)
        test <@ uid = Guid.Empty @>
    | Ok _ -> Assert.Fail("Unexpected route structure")
    | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

[<Fact>]
let ``OptPre without skip still requires authentication`` () =
    let ctx = createMockContextWithRoute []
    // No auth header - should fail because ChildRoute.Normal doesn't skip
    let pipeline =
        hydrateParentWithOptPre () (ParentWithOptPre.Children(OptPre(UserId Guid.Empty), ChildRoute.Normal))

    let result = pipeline ctx
    Assert.Equal(Error NotAuthenticated, result)

[<Fact>]
let ``Pre is NOT affected by SkipAllPreconditions`` () =
    let ctx = createMockContextWithRoute []
    // No auth headers - Pre<AdminId> should still fail even with SkipAllPreconditions on child
    let pipeline =
        hydrateParentWithBothPre
            ()
            (ParentWithBothPre.Children(Pre(AdminId Guid.Empty), OptPre(UserId Guid.Empty), ChildRoute.Public))

    let result = pipeline ctx
    // Should fail because Pre<AdminId> always runs (not skippable)
    Assert.Equal(Error Forbidden, result)

[<Fact>]
let ``Pre runs but OptPre skipped with SkipAllPreconditions`` () =
    let adminId = Guid.NewGuid()
    let ctx = createMockContextWithRoute []
    ctx.Request.Headers.Append("X-Admin-Id", adminId.ToString())
    // Admin auth provided, but no user auth - OptPre should be skipped
    let pipeline =
        hydrateParentWithBothPre
            ()
            (ParentWithBothPre.Children(Pre(AdminId Guid.Empty), OptPre(UserId Guid.Empty), ChildRoute.Public))

    let result = pipeline ctx

    match result with
    | Ok(ParentWithBothPre.Children(Pre(AdminId aid), OptPre(UserId uid), ChildRoute.Public)) ->
        test <@ aid = adminId @>
        test <@ uid = Guid.Empty @> // Sentinel value
    | Ok _ -> Assert.Fail("Unexpected route structure")
    | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

[<Fact>]
let ``SkipPrecondition skips specific OptPre type`` () =
    let ctx = createMockContextWithRoute []
    // No auth header - OptPre<UserId> should be skipped for PartiallyPublic
    let pipeline =
        hydrateParentWithOptPre () (ParentWithOptPre.Children(OptPre(UserId Guid.Empty), ChildRoute.PartiallyPublic))

    let result = pipeline ctx

    match result with
    | Ok(ParentWithOptPre.Children(OptPre(UserId uid), ChildRoute.PartiallyPublic)) -> test <@ uid = Guid.Empty @> // Sentinel value
    | Ok _ -> Assert.Fail("Unexpected route structure")
    | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

[<Fact>]
let ``Both Pre and OptPre work when both headers provided`` () =
    let adminId = Guid.NewGuid()
    let userId = Guid.NewGuid()
    let ctx = createMockContextWithRoute []
    ctx.Request.Headers.Append("X-Admin-Id", adminId.ToString())
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())

    let pipeline =
        hydrateParentWithBothPre
            ()
            (ParentWithBothPre.Children(Pre(AdminId Guid.Empty), OptPre(UserId Guid.Empty), ChildRoute.Normal))

    let result = pipeline ctx

    match result with
    | Ok(ParentWithBothPre.Children(Pre(AdminId aid), OptPre(UserId uid), ChildRoute.Normal)) ->
        test <@ aid = adminId @>
        test <@ uid = userId @>
    | Ok _ -> Assert.Fail("Unexpected route structure")
    | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

// =============================================================================
// Precondition validation tests
// =============================================================================

/// Route with Pre<UserId> that requires a precondition
type RouteNeedingPrecondition =
    | Public
    | Private of Pre<UserId>

[<Fact>]
let ``validatePreconditions returns Ok when all preconditions registered`` () =
    let preconditions = [ RouteHydration.forPre<UserId, TestError> mockUserAuth ]

    let result =
        RouteHydration.validatePreconditions<RouteNeedingPrecondition, TestError> preconditions

    test <@ result = Ok() @>

[<Fact>]
let ``validatePreconditions returns Error when precondition missing`` () =
    let preconditions: Precondition<TestError> list = []

    let result =
        RouteHydration.validatePreconditions<RouteNeedingPrecondition, TestError> preconditions

    match result with
    | Error errors -> test <@ errors |> List.exists (fun e -> e.Contains("Missing preconditions")) @>
    | Ok() -> failwith "Expected validation error for missing precondition"

/// Route with multiple precondition types
type RouteWithMultiplePreconditions =
    | Admin of Pre<AdminId>
    | User of Pre<UserId>
    | Both of Pre<AdminId> * Pre<UserId>

[<Fact>]
let ``validatePreconditions catches all missing preconditions`` () =
    let preconditions: Precondition<TestError> list = []

    let result =
        RouteHydration.validatePreconditions<RouteWithMultiplePreconditions, TestError> preconditions

    match result with
    | Error errors ->
        test
            <@
                errors
                |> List.exists (fun e -> e.Contains("Pre<UserId>") || e.Contains("Pre<AdminId>"))
            @>
    | Ok() -> failwith "Expected validation error for missing preconditions"

[<Fact>]
let ``validatePreconditions passes when all multiple preconditions registered`` () =
    let preconditions =
        [ RouteHydration.forPre<UserId, TestError> mockUserAuth
          RouteHydration.forPre<AdminId, TestError> mockAdminAuth ]

    let result =
        RouteHydration.validatePreconditions<RouteWithMultiplePreconditions, TestError> preconditions

    test <@ result = Ok() @>

/// Route with OptPre<UserId>
type RouteWithOptPre =
    | Normal of OptPre<UserId>
    | [<SkipAllPreconditions>] Public

[<Fact>]
let ``validatePreconditions requires OptPre preconditions too`` () =
    let preconditions: Precondition<TestError> list = []

    let result =
        RouteHydration.validatePreconditions<RouteWithOptPre, TestError> preconditions

    match result with
    | Error errors -> test <@ errors |> List.exists (fun e -> e.Contains("Missing preconditions")) @>
    | Ok() -> failwith "Expected validation error for missing OptPre precondition"

[<Fact>]
let ``validatePreconditions passes with OptPre precondition registered`` () =
    let preconditions = [ RouteHydration.forOptPre<UserId, TestError> mockUserAuth ]

    let result =
        RouteHydration.validatePreconditions<RouteWithOptPre, TestError> preconditions

    test <@ result = Ok() @>

// =============================================================================
// Full validation tests (structure + preconditions)
// =============================================================================

[<Fact>]
let ``validate combines structure and precondition validation`` () =
    let preconditions = [ RouteHydration.forPre<UserId, TestError> mockUserAuth ]

    let result =
        RouteHydration.validate<RouteNeedingPrecondition, TestError> preconditions

    test <@ result = Ok() @>

[<Fact>]
let ``validate catches missing preconditions`` () =
    let preconditions: Precondition<TestError> list = []

    let result =
        RouteHydration.validate<RouteNeedingPrecondition, TestError> preconditions

    match result with
    | Error errors -> test <@ errors |> List.exists (fun e -> e.Contains("Missing preconditions")) @>
    | Ok() -> failwith "Expected validation error"
