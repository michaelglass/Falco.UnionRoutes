module Falco.UnionRoutes.Tests.RouteHydrationTests

open System
open System.Collections.Generic
open System.Threading.Tasks
open Xunit
open Swensen.Unquote
open Falco.UnionRoutes
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives

// =============================================================================
// Test types
// =============================================================================

type UserId = UserId of Guid
type AdminId = AdminId of Guid

type TestError =
    | NotAuthenticated
    | Forbidden
    | BadRequest of string

// Routes using PreCondition<'T> for preconditions
type SimpleRoute =
    | List
    | Detail of id: Guid
    | WithAuth of PreCondition<UserId>
    | WithBoth of PreCondition<UserId> * id: Guid

type StringRoute = BySlug of slug: string

type IntRoute = ByPage of page: int

type Int64Route = ByBigId of id: int64

type BoolRoute = ByEnabled of enabled: bool

// Wrapper types (single-case DUs)
type PostId = PostId of Guid

type WrapperRoute =
    | ByPostId of id: PostId
    | WithAuthAndPostId of PreCondition<UserId> * id: PostId

// Custom type for testing custom extractors
type Slug = Slug of string

type CustomTypeRoute =
    | BySlugCustom of slug: Slug
    | WithAuthAndSlug of PreCondition<UserId> * slug: Slug

// Route without preconditions
type NoAuthRoute =
    | SimpleNoAuth of id: Guid
    | WithWrapper of id: PostId

// Routes for testing QueryParam<'T> (query string parameters)
type QueryRoute =
    | WithQueryString of query: QueryParam<string>
    | WithQueryInt of page: QueryParam<int>
    | WithOptionalQuery of query: QueryParam<string> option
    | WithOptionalQueryInt of page: QueryParam<int> option
    | MixedRouteAndQuery of id: Guid * sort: QueryParam<string>
    | MixedWithOptional of id: Guid * page: QueryParam<int> option

// Route for testing error accumulation
type MultiFieldRoute = TwoRequired of a: Guid * b: int

// Route for testing multiple preconditions
type MultiPreRoute =
    | NeedsUser of PreCondition<UserId>
    | NeedsAdmin of PreCondition<AdminId>
    | NeedsBoth of PreCondition<UserId> * PreCondition<AdminId>

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

let mockUserAuth: Extractor<UserId, TestError> =
    fun ctx ->
        Task.FromResult(
            match ctx.Request.Headers.TryGetValue("X-User-Id") with
            | true, values ->
                match Guid.TryParse(values.ToString()) with
                | true, guid -> Ok(UserId guid)
                | false, _ -> Error NotAuthenticated
            | false, _ -> Error NotAuthenticated
        )

let mockAdminAuth: Extractor<AdminId, TestError> =
    fun ctx ->
        Task.FromResult(
            match ctx.Request.Headers.TryGetValue("X-Admin-Id") with
            | true, values ->
                match Guid.TryParse(values.ToString()) with
                | true, guid -> Ok(AdminId guid)
                | false, _ -> Error Forbidden
            | false, _ -> Error Forbidden
        )

/// Convert extraction error string to TestError
let makeError (msg: string) = BadRequest msg

/// Combine multiple errors - preserves structure for pattern matching
let combineErrors (errors: TestError list) =
    match errors with
    | [ single ] -> single
    | multiple -> BadRequest(multiple |> List.map string |> String.concat "; ")

// Precondition factories (create fresh each time to avoid module initialization issues)
let userPrecondition () =
    Extractor.precondition<UserId, TestError> mockUserAuth

let adminPrecondition () =
    Extractor.precondition<AdminId, TestError> mockAdminAuth

let hydrate () =
    Route.extractor<SimpleRoute, TestError> [ userPrecondition () ] [] makeError combineErrors

let hydrateString () =
    Route.extractor<StringRoute, TestError> [] [] makeError combineErrors

let hydrateInt () =
    Route.extractor<IntRoute, TestError> [] [] makeError combineErrors

let hydrateInt64 () =
    Route.extractor<Int64Route, TestError> [] [] makeError combineErrors

let hydrateBool () =
    Route.extractor<BoolRoute, TestError> [] [] makeError combineErrors

let hydrateWrapper () =
    Route.extractor<WrapperRoute, TestError> [ userPrecondition () ] [] makeError combineErrors

// Custom parser for Slug type
let slugParser: Parser<Slug> = fun s -> Ok(Slug s)

let hydrateCustom () =
    Route.extractor<CustomTypeRoute, TestError>
        [ userPrecondition () ]
        [ Extractor.parser slugParser ]
        makeError
        combineErrors

let hydrateNoAuth () =
    Route.extractor<NoAuthRoute, TestError> [] [] makeError combineErrors

let hydrateNoAuthWithCustom () =
    Route.extractor<CustomTypeRoute, TestError> [] [ Extractor.parser slugParser ] makeError combineErrors

let hydrateQuery () =
    Route.extractor<QueryRoute, TestError> [] [] makeError combineErrors

let hydrateMultiField () =
    Route.extractor<MultiFieldRoute, TestError> [] [] makeError combineErrors

let hydrateMultiPre () =
    Route.extractor<MultiPreRoute, TestError> [ userPrecondition (); adminPrecondition () ] [] makeError combineErrors

// =============================================================================
// Unit route tests (no fields)
// =============================================================================

[<Fact>]
let ``hydrates unit route with no changes`` () =
    task {
        let ctx = createMockContextWithRoute []
        let pipeline = hydrate () SimpleRoute.List
        let! result = pipeline ctx
        test <@ result = Ok SimpleRoute.List @>
    }

// =============================================================================
// Guid field tests
// =============================================================================

[<Fact>]
let ``hydrates Guid field from route params`` () =
    task {
        let id = Guid.NewGuid()
        let ctx = createMockContextWithRoute [ ("id", id.ToString()) ]
        let pipeline = hydrate () (SimpleRoute.Detail Guid.Empty)
        let! result = pipeline ctx
        test <@ result = Ok(SimpleRoute.Detail id) @>
    }

[<Fact>]
let ``returns error for invalid Guid`` () =
    task {
        let ctx = createMockContextWithRoute [ ("id", "not-a-guid") ]
        let pipeline = hydrate () (SimpleRoute.Detail Guid.Empty)
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

[<Fact>]
let ``returns error for missing Guid param`` () =
    task {
        let ctx = createMockContextWithRoute []
        let pipeline = hydrate () (SimpleRoute.Detail Guid.Empty)
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

[<Fact>]
let ``error message contains field name for missing param`` () =
    task {
        let ctx = createMockContextWithRoute []
        let pipeline = hydrate () (SimpleRoute.Detail Guid.Empty)
        let! result = pipeline ctx

        match result with
        | Error(BadRequest msg) -> Assert.Contains("id", msg)
        | Error NotAuthenticated -> Assert.Fail("Unexpected NotAuthenticated")
        | Error Forbidden -> Assert.Fail("Unexpected Forbidden")
        | Ok _ -> Assert.Fail("Expected error")
    }

// =============================================================================
// Precondition (Pre<'T>) field tests
// =============================================================================

[<Fact>]
let ``hydrates PreCondition field from precondition`` () =
    task {
        let userId = Guid.NewGuid()
        let ctx = createMockContextWithRoute []
        ctx.Request.Headers.Append("X-User-Id", userId.ToString())
        let pipeline = hydrate () (SimpleRoute.WithAuth(PreCondition(UserId Guid.Empty)))
        let! result = pipeline ctx
        Assert.Equal(Ok(SimpleRoute.WithAuth(PreCondition(UserId userId))), result)
    }

[<Fact>]
let ``returns precondition error when not authenticated`` () =
    task {
        let ctx = createMockContextWithRoute []
        let pipeline = hydrate () (SimpleRoute.WithAuth(PreCondition(UserId Guid.Empty)))
        let! result = pipeline ctx
        // Precondition errors preserve their original type
        Assert.Equal(Error NotAuthenticated, result)
    }

// =============================================================================
// Combined field tests
// =============================================================================

[<Fact>]
let ``hydrates both PreCondition and Guid fields`` () =
    task {
        let userId = Guid.NewGuid()
        let postId = Guid.NewGuid()
        let ctx = createMockContextWithRoute [ ("id", postId.ToString()) ]
        ctx.Request.Headers.Append("X-User-Id", userId.ToString())

        let pipeline =
            hydrate () (SimpleRoute.WithBoth(PreCondition(UserId Guid.Empty), Guid.Empty))

        let! result = pipeline ctx
        Assert.Equal(Ok(SimpleRoute.WithBoth(PreCondition(UserId userId), postId)), result)
    }

[<Fact>]
let ``returns precondition error even with valid Guid when not authenticated`` () =
    task {
        let postId = Guid.NewGuid()
        let ctx = createMockContextWithRoute [ ("id", postId.ToString()) ]

        let pipeline =
            hydrate () (SimpleRoute.WithBoth(PreCondition(UserId Guid.Empty), Guid.Empty))

        let! result = pipeline ctx
        // Precondition errors preserve their original type
        Assert.Equal(Error NotAuthenticated, result)
    }

[<Fact>]
let ``returns Guid error with valid precondition but invalid Guid`` () =
    task {
        let userId = Guid.NewGuid()
        let ctx = createMockContextWithRoute [ ("id", "invalid") ]
        ctx.Request.Headers.Append("X-User-Id", userId.ToString())

        let pipeline =
            hydrate () (SimpleRoute.WithBoth(PreCondition(UserId Guid.Empty), Guid.Empty))

        let! result = pipeline ctx
        Assert.True(Result.isError result)
    }

// =============================================================================
// Multiple preconditions tests
// =============================================================================

[<Fact>]
let ``hydrates PreCondition<UserId> with user precondition`` () =
    task {
        let userId = Guid.NewGuid()
        let ctx = createMockContextWithRoute []
        ctx.Request.Headers.Append("X-User-Id", userId.ToString())

        let pipeline =
            hydrateMultiPre () (MultiPreRoute.NeedsUser(PreCondition(UserId Guid.Empty)))

        let! result = pipeline ctx
        Assert.Equal(Ok(MultiPreRoute.NeedsUser(PreCondition(UserId userId))), result)
    }

[<Fact>]
let ``hydrates PreCondition<AdminId> with admin precondition`` () =
    task {
        let adminId = Guid.NewGuid()
        let ctx = createMockContextWithRoute []
        ctx.Request.Headers.Append("X-Admin-Id", adminId.ToString())

        let pipeline =
            hydrateMultiPre () (MultiPreRoute.NeedsAdmin(PreCondition(AdminId Guid.Empty)))

        let! result = pipeline ctx
        Assert.Equal(Ok(MultiPreRoute.NeedsAdmin(PreCondition(AdminId adminId))), result)
    }

[<Fact>]
let ``hydrates both PreCondition<UserId> and PreCondition<AdminId>`` () =
    task {
        let userId = Guid.NewGuid()
        let adminId = Guid.NewGuid()
        let ctx = createMockContextWithRoute []
        ctx.Request.Headers.Append("X-User-Id", userId.ToString())
        ctx.Request.Headers.Append("X-Admin-Id", adminId.ToString())

        let pipeline =
            hydrateMultiPre
                ()
                (MultiPreRoute.NeedsBoth(PreCondition(UserId Guid.Empty), PreCondition(AdminId Guid.Empty)))

        let! result = pipeline ctx
        Assert.Equal(Ok(MultiPreRoute.NeedsBoth(PreCondition(UserId userId), PreCondition(AdminId adminId))), result)
    }

[<Fact>]
let ``returns correct error for missing user precondition`` () =
    task {
        let ctx = createMockContextWithRoute []

        let pipeline =
            hydrateMultiPre () (MultiPreRoute.NeedsUser(PreCondition(UserId Guid.Empty)))

        let! result = pipeline ctx
        Assert.Equal(Error NotAuthenticated, result)
    }

[<Fact>]
let ``returns correct error for missing admin precondition`` () =
    task {
        let ctx = createMockContextWithRoute []

        let pipeline =
            hydrateMultiPre () (MultiPreRoute.NeedsAdmin(PreCondition(AdminId Guid.Empty)))

        let! result = pipeline ctx
        Assert.Equal(Error Forbidden, result)
    }

[<Fact>]
let ``accumulates errors when both preconditions fail`` () =
    task {
        let ctx = createMockContextWithRoute []
        // Neither user nor admin headers set
        let pipeline =
            hydrateMultiPre
                ()
                (MultiPreRoute.NeedsBoth(PreCondition(UserId Guid.Empty), PreCondition(AdminId Guid.Empty)))

        let! result = pipeline ctx
        // combineErrors should combine both errors
        match result with
        | Error(BadRequest msg) ->
            // Both NotAuthenticated and Forbidden should be in the combined error
            Assert.Contains("NotAuthenticated", msg)
            Assert.Contains("Forbidden", msg)
        | Error NotAuthenticated -> Assert.Fail("Expected combined error, got single NotAuthenticated")
        | Error Forbidden -> Assert.Fail("Expected combined error, got single Forbidden")
        | Ok _ -> Assert.Fail("Expected error, got Ok")
    }

// =============================================================================
// String field tests
// =============================================================================

[<Fact>]
let ``hydrates string field from route params`` () =
    task {
        let ctx = createMockContextWithRoute [ ("slug", "hello-world") ]
        let pipeline = hydrateString () (StringRoute.BySlug "")
        let! result = pipeline ctx
        test <@ result = Ok(StringRoute.BySlug "hello-world") @>
    }

[<Fact>]
let ``returns error for empty string param`` () =
    task {
        let ctx = createMockContextWithRoute [ ("slug", "") ]
        let pipeline = hydrateString () (StringRoute.BySlug "")
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

[<Fact>]
let ``returns error for missing string param`` () =
    task {
        let ctx = createMockContextWithRoute []
        let pipeline = hydrateString () (StringRoute.BySlug "")
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

// =============================================================================
// Int field tests
// =============================================================================

[<Fact>]
let ``hydrates int field from route params`` () =
    task {
        let ctx = createMockContextWithRoute [ ("page", "42") ]
        let pipeline = hydrateInt () (IntRoute.ByPage 0)
        let! result = pipeline ctx
        test <@ result = Ok(IntRoute.ByPage 42) @>
    }

[<Fact>]
let ``handles negative int`` () =
    task {
        let ctx = createMockContextWithRoute [ ("page", "-5") ]
        let pipeline = hydrateInt () (IntRoute.ByPage 0)
        let! result = pipeline ctx
        test <@ result = Ok(IntRoute.ByPage -5) @>
    }

[<Fact>]
let ``returns error for non-numeric int param`` () =
    task {
        let ctx = createMockContextWithRoute [ ("page", "abc") ]
        let pipeline = hydrateInt () (IntRoute.ByPage 0)
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

[<Fact>]
let ``handles zero int`` () =
    task {
        let ctx = createMockContextWithRoute [ ("page", "0") ]
        let pipeline = hydrateInt () (IntRoute.ByPage 1)
        let! result = pipeline ctx
        test <@ result = Ok(IntRoute.ByPage 0) @>
    }

[<Fact>]
let ``handles Int32.MaxValue`` () =
    task {
        let ctx = createMockContextWithRoute [ ("page", "2147483647") ]
        let pipeline = hydrateInt () (IntRoute.ByPage 0)
        let! result = pipeline ctx
        test <@ result = Ok(IntRoute.ByPage 2147483647) @>
    }

[<Fact>]
let ``handles Int32.MinValue`` () =
    task {
        let ctx = createMockContextWithRoute [ ("page", "-2147483648") ]
        let pipeline = hydrateInt () (IntRoute.ByPage 0)
        let! result = pipeline ctx
        test <@ result = Ok(IntRoute.ByPage -2147483648) @>
    }

[<Fact>]
let ``returns error for int overflow`` () =
    task {
        let ctx = createMockContextWithRoute [ ("page", "2147483648") ] // Int32.MaxValue + 1
        let pipeline = hydrateInt () (IntRoute.ByPage 0)
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

// =============================================================================
// Int64 field tests
// =============================================================================

[<Fact>]
let ``hydrates int64 field from route params`` () =
    task {
        let ctx = createMockContextWithRoute [ ("id", "1234567890") ]
        let pipeline = hydrateInt64 () (Int64Route.ByBigId 0L)
        let! result = pipeline ctx
        test <@ result = Ok(Int64Route.ByBigId 1234567890L) @>
    }

[<Fact>]
let ``hydrates negative int64 field`` () =
    task {
        // Note: "-9876543210" is outside int32 range, testing true int64 support
        let ctx = createMockContextWithRoute [ ("id", "-9876543210") ]
        let pipeline = hydrateInt64 () (Int64Route.ByBigId 0L)
        let! result = pipeline ctx
        test <@ result = Ok(Int64Route.ByBigId -9876543210L) @>
    }

[<Fact>]
let ``hydrates Int64.MaxValue from route params`` () =
    task {
        let ctx = createMockContextWithRoute [ ("id", "9223372036854775807") ]
        let pipeline = hydrateInt64 () (Int64Route.ByBigId 0L)
        let! result = pipeline ctx
        test <@ result = Ok(Int64Route.ByBigId 9223372036854775807L) @>
    }

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
    task {
        let ctx = createMockContextWithRoute [ ("id", "abc") ]
        let pipeline = hydrateInt64 () (Int64Route.ByBigId 0L)
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

// =============================================================================
// Bool field tests
// =============================================================================

[<Fact>]
let ``hydrates bool field from route params - true`` () =
    task {
        let ctx = createMockContextWithRoute [ ("enabled", "true") ]
        let pipeline = hydrateBool () (BoolRoute.ByEnabled false)
        let! result = pipeline ctx
        test <@ result = Ok(BoolRoute.ByEnabled true) @>
    }

[<Fact>]
let ``hydrates bool field from route params - false`` () =
    task {
        let ctx = createMockContextWithRoute [ ("enabled", "false") ]
        let pipeline = hydrateBool () (BoolRoute.ByEnabled true)
        let! result = pipeline ctx
        test <@ result = Ok(BoolRoute.ByEnabled false) @>
    }

[<Fact>]
let ``returns error for invalid bool param`` () =
    task {
        let ctx = createMockContextWithRoute [ ("enabled", "yes") ]
        let pipeline = hydrateBool () (BoolRoute.ByEnabled false)
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

// =============================================================================
// Wrapper type tests (single-case DUs like PostId)
// =============================================================================

[<Fact>]
let ``hydrates wrapper type field from route params`` () =
    task {
        let id = Guid.NewGuid()
        let ctx = createMockContextWithRoute [ ("id", id.ToString()) ]
        let pipeline = hydrateWrapper () (WrapperRoute.ByPostId(PostId Guid.Empty))
        let! result = pipeline ctx
        Assert.Equal(Ok(WrapperRoute.ByPostId(PostId id)), result)
    }

[<Fact>]
let ``returns error for invalid Guid in wrapper type`` () =
    task {
        let ctx = createMockContextWithRoute [ ("id", "not-a-guid") ]
        let pipeline = hydrateWrapper () (WrapperRoute.ByPostId(PostId Guid.Empty))
        let! result = pipeline ctx
        Assert.True(Result.isError result)
    }

[<Fact>]
let ``hydrates both PreCondition and wrapper type fields`` () =
    task {
        let userId = Guid.NewGuid()
        let postId = Guid.NewGuid()
        let ctx = createMockContextWithRoute [ ("id", postId.ToString()) ]
        ctx.Request.Headers.Append("X-User-Id", userId.ToString())

        let pipeline =
            hydrateWrapper () (WrapperRoute.WithAuthAndPostId(PreCondition(UserId Guid.Empty), PostId Guid.Empty))

        let! result = pipeline ctx
        Assert.Equal(Ok(WrapperRoute.WithAuthAndPostId(PreCondition(UserId userId), PostId postId)), result)
    }

// =============================================================================
// Custom extractor tests
// =============================================================================

[<Fact>]
let ``custom extractor handles custom type`` () =
    task {
        let ctx = createMockContextWithRoute [ ("slug", "hello-world") ]
        let pipeline = hydrateNoAuthWithCustom () (CustomTypeRoute.BySlugCustom(Slug ""))
        let! result = pipeline ctx
        test <@ result = Ok(CustomTypeRoute.BySlugCustom(Slug "hello-world")) @>
    }

[<Fact>]
let ``custom extractor returns error for missing value`` () =
    task {
        let ctx = createMockContextWithRoute []
        let pipeline = hydrateNoAuthWithCustom () (CustomTypeRoute.BySlugCustom(Slug ""))
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

[<Fact>]
let ``custom extractor works with PreCondition`` () =
    task {
        let userId = Guid.NewGuid()
        let ctx = createMockContextWithRoute [ ("slug", "my-post") ]
        ctx.Request.Headers.Append("X-User-Id", userId.ToString())

        let pipeline =
            hydrateCustom () (CustomTypeRoute.WithAuthAndSlug(PreCondition(UserId Guid.Empty), Slug ""))

        let! result = pipeline ctx
        Assert.Equal(Ok(CustomTypeRoute.WithAuthAndSlug(PreCondition(UserId userId), Slug "my-post")), result)
    }

// =============================================================================
// No precondition tests
// =============================================================================

[<Fact>]
let ``hydrates Guid field without preconditions`` () =
    task {
        let id = Guid.NewGuid()
        let ctx = createMockContextWithRoute [ ("id", id.ToString()) ]
        let pipeline = hydrateNoAuth () (NoAuthRoute.SimpleNoAuth Guid.Empty)
        let! result = pipeline ctx
        test <@ result = Ok(NoAuthRoute.SimpleNoAuth id) @>
    }

[<Fact>]
let ``hydrates wrapper type without preconditions`` () =
    task {
        let id = Guid.NewGuid()
        let ctx = createMockContextWithRoute [ ("id", id.ToString()) ]
        let pipeline = hydrateNoAuth () (NoAuthRoute.WithWrapper(PostId Guid.Empty))
        let! result = pipeline ctx
        test <@ result = Ok(NoAuthRoute.WithWrapper(PostId id)) @>
    }

// =============================================================================
// Query parameter tests
// =============================================================================

[<Fact>]
let ``QueryParam string field extracts from query`` () =
    task {
        let ctx = createMockContextWithRouteAndQuery [] [ ("query", "hello") ]
        let pipeline = hydrateQuery () (QueryRoute.WithQueryString(QueryParam ""))
        let! result = pipeline ctx
        test <@ result = Ok(QueryRoute.WithQueryString(QueryParam "hello")) @>
    }

[<Fact>]
let ``QueryParam int field extracts from query`` () =
    task {
        let ctx = createMockContextWithRouteAndQuery [] [ ("page", "10") ]
        let pipeline = hydrateQuery () (QueryRoute.WithQueryInt(QueryParam 0))
        let! result = pipeline ctx
        test <@ result = Ok(QueryRoute.WithQueryInt(QueryParam 10)) @>
    }

[<Fact>]
let ``QueryParam field returns error when missing`` () =
    task {
        let ctx = createMockContextWithRouteAndQuery [] []
        let pipeline = hydrateQuery () (QueryRoute.WithQueryString(QueryParam ""))
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

[<Fact>]
let ``optional QueryParam field returns Some when present`` () =
    task {
        let ctx = createMockContextWithRouteAndQuery [] [ ("query", "search") ]
        let pipeline = hydrateQuery () (QueryRoute.WithOptionalQuery None)
        let! result = pipeline ctx
        test <@ result = Ok(QueryRoute.WithOptionalQuery(Some(QueryParam "search"))) @>
    }

[<Fact>]
let ``optional QueryParam field returns None when missing`` () =
    task {
        let ctx = createMockContextWithRouteAndQuery [] []
        let pipeline = hydrateQuery () (QueryRoute.WithOptionalQuery None)
        let! result = pipeline ctx
        test <@ result = Ok(QueryRoute.WithOptionalQuery None) @>
    }

[<Fact>]
let ``mixed route and query parameters`` () =
    task {
        let id = Guid.NewGuid()

        let ctx =
            createMockContextWithRouteAndQuery [ ("id", id.ToString()) ] [ ("sort", "date") ]

        let pipeline =
            hydrateQuery () (QueryRoute.MixedRouteAndQuery(Guid.Empty, QueryParam ""))

        let! result = pipeline ctx
        test <@ result = Ok(QueryRoute.MixedRouteAndQuery(id, QueryParam "date")) @>
    }

[<Fact>]
let ``optional QueryParam int returns Some when present`` () =
    task {
        let ctx = createMockContextWithRouteAndQuery [] [ ("page", "42") ]
        let pipeline = hydrateQuery () (QueryRoute.WithOptionalQueryInt None)
        let! result = pipeline ctx
        test <@ result = Ok(QueryRoute.WithOptionalQueryInt(Some(QueryParam 42))) @>
    }

[<Fact>]
let ``optional QueryParam int returns None when missing`` () =
    task {
        let ctx = createMockContextWithRouteAndQuery [] []
        let pipeline = hydrateQuery () (QueryRoute.WithOptionalQueryInt None)
        let! result = pipeline ctx
        test <@ result = Ok(QueryRoute.WithOptionalQueryInt None) @>
    }

[<Fact>]
let ``optional QueryParam int returns error for invalid value`` () =
    task {
        let ctx = createMockContextWithRouteAndQuery [] [ ("page", "abc") ]
        let pipeline = hydrateQuery () (QueryRoute.WithOptionalQueryInt None)
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

[<Fact>]
let ``mixed route param and optional query`` () =
    task {
        let id = Guid.NewGuid()

        let ctx =
            createMockContextWithRouteAndQuery [ ("id", id.ToString()) ] [ ("page", "5") ]

        let pipeline = hydrateQuery () (QueryRoute.MixedWithOptional(Guid.Empty, None))
        let! result = pipeline ctx
        test <@ result = Ok(QueryRoute.MixedWithOptional(id, Some(QueryParam 5))) @>
    }

[<Fact>]
let ``mixed route param works without optional query`` () =
    task {
        let id = Guid.NewGuid()
        let ctx = createMockContextWithRouteAndQuery [ ("id", id.ToString()) ] []
        let pipeline = hydrateQuery () (QueryRoute.MixedWithOptional(Guid.Empty, None))
        let! result = pipeline ctx
        test <@ result = Ok(QueryRoute.MixedWithOptional(id, None)) @>
    }

// =============================================================================
// Error accumulation tests
// =============================================================================

[<Fact>]
let ``hydration collects all errors`` () =
    task {
        let ctx = createMockContextWithRoute []
        let pipeline = hydrateMultiField () (MultiFieldRoute.TwoRequired(Guid.Empty, 0))
        let! result = pipeline ctx

        match result with
        | Error(BadRequest msg) -> test <@ msg.Contains("a") && msg.Contains("b") @>
        | Error NotAuthenticated -> Assert.Fail("Unexpected NotAuthenticated error")
        | Error Forbidden -> Assert.Fail("Unexpected Forbidden error")
        | Ok route -> Assert.Fail($"Expected error but got: {route}")
    }

[<Fact>]
let ``hydration succeeds when all fields valid`` () =
    task {
        let id = Guid.NewGuid()
        let ctx = createMockContextWithRoute [ ("a", id.ToString()); ("b", "42") ]
        let pipeline = hydrateMultiField () (MultiFieldRoute.TwoRequired(Guid.Empty, 0))
        let! result = pipeline ctx
        test <@ result = Ok(MultiFieldRoute.TwoRequired(id, 42)) @>
    }

// =============================================================================
// OverridablePreCondition<'T> tests (skippable preconditions)
// =============================================================================

// Routes using OverridablePreCondition<'T> for skippable preconditions
type OverridablePreConditionRoute =
    | WithOptAuth of OverridablePreCondition<UserId>
    | WithBothPreTypes of PreCondition<AdminId> * OverridablePreCondition<UserId>
    | WithOptAuthAndId of OverridablePreCondition<UserId> * id: Guid

// Nested routes demonstrating skip behavior
type ChildRoute =
    | Normal
    | [<SkipAllPreconditions>] Public
    | [<SkipPrecondition(typeof<UserId>)>] PartiallyPublic

type ParentWithOverridablePreCondition = Children of OverridablePreCondition<UserId> * ChildRoute

type ParentWithBothPreConditions = Children of PreCondition<AdminId> * OverridablePreCondition<UserId> * ChildRoute

let overridablePreCondition () =
    Extractor.overridablePrecondition<UserId, TestError> mockUserAuth

let hydrateOverridablePreCondition () =
    Route.extractor<OverridablePreConditionRoute, TestError> [ overridablePreCondition () ] [] makeError combineErrors

let hydrateOverridablePreConditionWithAdmin () =
    Route.extractor<OverridablePreConditionRoute, TestError>
        [ overridablePreCondition (); adminPrecondition () ]
        []
        makeError
        combineErrors

let hydrateParentWithOverridablePreCondition () =
    Route.extractor<ParentWithOverridablePreCondition, TestError>
        [ overridablePreCondition () ]
        []
        makeError
        combineErrors

let hydrateParentWithBothPreConditions () =
    Route.extractor<ParentWithBothPreConditions, TestError>
        [ adminPrecondition (); overridablePreCondition () ]
        []
        makeError
        combineErrors

[<Fact>]
let ``OverridablePreCondition field hydrates from precondition when authenticated`` () =
    task {
        let userId = Guid.NewGuid()
        let ctx = createMockContextWithRoute []
        ctx.Request.Headers.Append("X-User-Id", userId.ToString())

        let pipeline =
            hydrateOverridablePreCondition
                ()
                (OverridablePreConditionRoute.WithOptAuth(OverridablePreCondition(UserId Guid.Empty)))

        let! result = pipeline ctx
        Assert.Equal(Ok(OverridablePreConditionRoute.WithOptAuth(OverridablePreCondition(UserId userId))), result)
    }

[<Fact>]
let ``OverridablePreCondition field returns error when not authenticated (without skip)`` () =
    task {
        let ctx = createMockContextWithRoute []

        let pipeline =
            hydrateOverridablePreCondition
                ()
                (OverridablePreConditionRoute.WithOptAuth(OverridablePreCondition(UserId Guid.Empty)))

        let! result = pipeline ctx
        Assert.Equal(Error NotAuthenticated, result)
    }

[<Fact>]
let ``OverridablePreCondition with SkipAllPreconditions provides sentinel value`` () =
    task {
        let ctx = createMockContextWithRoute []
        // No auth header - would normally fail
        let pipeline =
            hydrateParentWithOverridablePreCondition
                ()
                (ParentWithOverridablePreCondition.Children(
                    OverridablePreCondition(UserId Guid.Empty),
                    ChildRoute.Public
                ))

        let! result = pipeline ctx
        // Should succeed because OverridablePreCondition is skipped - provides sentinel value
        match result with
        | Ok(ParentWithOverridablePreCondition.Children(OverridablePreCondition(UserId uid), ChildRoute.Public)) ->
            // Sentinel value should be Guid.Empty (default for value type inside wrapper)
            test <@ uid = Guid.Empty @>
        | Ok _ -> Assert.Fail("Unexpected route structure")
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")
    }

[<Fact>]
let ``OverridablePreCondition without skip still requires authentication`` () =
    task {
        let ctx = createMockContextWithRoute []
        // No auth header - should fail because ChildRoute.Normal doesn't skip
        let pipeline =
            hydrateParentWithOverridablePreCondition
                ()
                (ParentWithOverridablePreCondition.Children(
                    OverridablePreCondition(UserId Guid.Empty),
                    ChildRoute.Normal
                ))

        let! result = pipeline ctx
        Assert.Equal(Error NotAuthenticated, result)
    }

[<Fact>]
let ``PreCondition is NOT affected by SkipAllPreconditions`` () =
    task {
        let ctx = createMockContextWithRoute []
        // No auth headers - PreCondition<AdminId> should still fail even with SkipAllPreconditions on child
        let pipeline =
            hydrateParentWithBothPreConditions
                ()
                (ParentWithBothPreConditions.Children(
                    PreCondition(AdminId Guid.Empty),
                    OverridablePreCondition(UserId Guid.Empty),
                    ChildRoute.Public
                ))

        let! result = pipeline ctx
        // Should fail because PreCondition<AdminId> always runs (not skippable)
        Assert.Equal(Error Forbidden, result)
    }

[<Fact>]
let ``PreCondition runs but OverridablePreCondition skipped with SkipAllPreconditions`` () =
    task {
        let adminId = Guid.NewGuid()
        let ctx = createMockContextWithRoute []
        ctx.Request.Headers.Append("X-Admin-Id", adminId.ToString())
        // Admin auth provided, but no user auth - OverridablePreCondition should be skipped
        let pipeline =
            hydrateParentWithBothPreConditions
                ()
                (ParentWithBothPreConditions.Children(
                    PreCondition(AdminId Guid.Empty),
                    OverridablePreCondition(UserId Guid.Empty),
                    ChildRoute.Public
                ))

        let! result = pipeline ctx

        match result with
        | Ok(ParentWithBothPreConditions.Children(PreCondition(AdminId aid),
                                                  OverridablePreCondition(UserId uid),
                                                  ChildRoute.Public)) ->
            test <@ aid = adminId @>
            test <@ uid = Guid.Empty @> // Sentinel value
        | Ok _ -> Assert.Fail("Unexpected route structure")
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")
    }

[<Fact>]
let ``SkipPrecondition skips specific OverridablePreCondition type`` () =
    task {
        let ctx = createMockContextWithRoute []
        // No auth header - OverridablePreCondition<UserId> should be skipped for PartiallyPublic
        let pipeline =
            hydrateParentWithOverridablePreCondition
                ()
                (ParentWithOverridablePreCondition.Children(
                    OverridablePreCondition(UserId Guid.Empty),
                    ChildRoute.PartiallyPublic
                ))

        let! result = pipeline ctx

        match result with
        | Ok(ParentWithOverridablePreCondition.Children(OverridablePreCondition(UserId uid), ChildRoute.PartiallyPublic)) ->
            test <@ uid = Guid.Empty @> // Sentinel value
        | Ok _ -> Assert.Fail("Unexpected route structure")
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")
    }

[<Fact>]
let ``Both PreCondition and OverridablePreCondition work when both headers provided`` () =
    task {
        let adminId = Guid.NewGuid()
        let userId = Guid.NewGuid()
        let ctx = createMockContextWithRoute []
        ctx.Request.Headers.Append("X-Admin-Id", adminId.ToString())
        ctx.Request.Headers.Append("X-User-Id", userId.ToString())

        let pipeline =
            hydrateParentWithBothPreConditions
                ()
                (ParentWithBothPreConditions.Children(
                    PreCondition(AdminId Guid.Empty),
                    OverridablePreCondition(UserId Guid.Empty),
                    ChildRoute.Normal
                ))

        let! result = pipeline ctx

        match result with
        | Ok(ParentWithBothPreConditions.Children(PreCondition(AdminId aid),
                                                  OverridablePreCondition(UserId uid),
                                                  ChildRoute.Normal)) ->
            test <@ aid = adminId @>
            test <@ uid = userId @>
        | Ok _ -> Assert.Fail("Unexpected route structure")
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")
    }

// =============================================================================
// Precondition validation tests
// =============================================================================

/// Route with PreCondition<UserId> that requires a precondition
type RouteNeedingPrecondition =
    | Public
    | Private of PreCondition<UserId>

[<Fact>]
let ``validatePreconditions returns Ok when all preconditions registered`` () =
    let preconditions = [ Extractor.precondition<UserId, TestError> mockUserAuth ]

    let result =
        Route.validatePreconditions<RouteNeedingPrecondition, TestError> preconditions

    test <@ result = Ok() @>

[<Fact>]
let ``validatePreconditions returns Error when precondition missing`` () =
    let preconditions: PreconditionExtractor<TestError> list = []

    let result =
        Route.validatePreconditions<RouteNeedingPrecondition, TestError> preconditions

    match result with
    | Error errors -> test <@ errors |> List.exists (fun e -> e.Contains("Missing preconditions")) @>
    | Ok() -> failwith "Expected validation error for missing precondition"

/// Route with multiple precondition types
type RouteWithMultiplePreconditions =
    | Admin of PreCondition<AdminId>
    | User of PreCondition<UserId>
    | Both of PreCondition<AdminId> * PreCondition<UserId>

[<Fact>]
let ``validatePreconditions catches all missing preconditions`` () =
    let preconditions: PreconditionExtractor<TestError> list = []

    let result =
        Route.validatePreconditions<RouteWithMultiplePreconditions, TestError> preconditions

    match result with
    | Error errors ->
        test
            <@
                errors
                |> List.exists (fun e -> e.Contains("PreCondition<UserId>") || e.Contains("PreCondition<AdminId>"))
            @>
    | Ok() -> failwith "Expected validation error for missing preconditions"

[<Fact>]
let ``validatePreconditions passes when all multiple preconditions registered`` () =
    let preconditions =
        [ Extractor.precondition<UserId, TestError> mockUserAuth
          Extractor.precondition<AdminId, TestError> mockAdminAuth ]

    let result =
        Route.validatePreconditions<RouteWithMultiplePreconditions, TestError> preconditions

    test <@ result = Ok() @>

/// Route with OverridablePreCondition<UserId>
type RouteWithOverridablePreCondition =
    | Normal of OverridablePreCondition<UserId>
    | [<SkipAllPreconditions>] Public

[<Fact>]
let ``validatePreconditions requires OverridablePreCondition preconditions too`` () =
    let preconditions: PreconditionExtractor<TestError> list = []

    let result =
        Route.validatePreconditions<RouteWithOverridablePreCondition, TestError> preconditions

    match result with
    | Error errors -> test <@ errors |> List.exists (fun e -> e.Contains("Missing preconditions")) @>
    | Ok() -> failwith "Expected validation error for missing OverridablePreCondition precondition"

[<Fact>]
let ``validatePreconditions passes with OverridablePreCondition precondition registered`` () =
    let preconditions =
        [ Extractor.overridablePrecondition<UserId, TestError> mockUserAuth ]

    let result =
        Route.validatePreconditions<RouteWithOverridablePreCondition, TestError> preconditions

    test <@ result = Ok() @>

// =============================================================================
// Full validation tests (structure + preconditions)
// =============================================================================

[<Fact>]
let ``validate combines structure and precondition validation`` () =
    let preconditions = [ Extractor.precondition<UserId, TestError> mockUserAuth ]

    let result = Route.validate<RouteNeedingPrecondition, TestError> preconditions

    test <@ result = Ok() @>

[<Fact>]
let ``validate catches missing preconditions`` () =
    let preconditions: PreconditionExtractor<TestError> list = []

    let result = Route.validate<RouteNeedingPrecondition, TestError> preconditions

    match result with
    | Error errors -> test <@ errors |> List.exists (fun e -> e.Contains("Missing preconditions")) @>
    | Ok() -> failwith "Expected validation error"

// =============================================================================
// Typed parser tests
// =============================================================================

type ToggleState =
    | On
    | Off

type ToggleRoute = | [<Route(Path = "toggle/{state}")>] Toggle of state: ToggleState

let toggleParser =
    Extractor.typedParser<bool, ToggleState> (fun b -> Ok(if b then On else Off))

let hydrateToggle () =
    Route.extractor<ToggleRoute, TestError> [] [ toggleParser ] makeError combineErrors

[<Fact>]
let ``typed parser receives pre-parsed bool value - true`` () =
    task {
        let ctx = createMockContextWithRoute [ ("state", "true") ]
        let pipeline = hydrateToggle () (ToggleRoute.Toggle Off)
        let! result = pipeline ctx
        test <@ result = Ok(ToggleRoute.Toggle On) @>
    }

[<Fact>]
let ``typed parser receives pre-parsed bool value - false`` () =
    task {
        let ctx = createMockContextWithRoute [ ("state", "false") ]
        let pipeline = hydrateToggle () (ToggleRoute.Toggle On)
        let! result = pipeline ctx
        test <@ result = Ok(ToggleRoute.Toggle Off) @>
    }

[<Fact>]
let ``typed parser returns error for invalid input`` () =
    task {
        let ctx = createMockContextWithRoute [ ("state", "maybe") ]
        let pipeline = hydrateToggle () (ToggleRoute.Toggle Off)
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

[<Fact>]
let ``typed parser returns error for missing param`` () =
    task {
        let ctx = createMockContextWithRoute []
        let pipeline = hydrateToggle () (ToggleRoute.Toggle Off)
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

// =============================================================================
// Constrained parser tests
// =============================================================================

type AlphaSlug = AlphaSlug of string

type AlphaSlugRoute = BySlug of slug: AlphaSlug

let alphaSlugParser =
    Extractor.constrainedParser<AlphaSlug> [| RouteConstraint.Alpha |] (fun s -> Ok(AlphaSlug s))

let hydrateAlphaSlug () =
    Route.extractor<AlphaSlugRoute, TestError> [] [ alphaSlugParser ] makeError combineErrors

[<Fact>]
let ``constrained parser hydrates string value`` () =
    task {
        let ctx = createMockContextWithRoute [ ("slug", "helloworld") ]
        let pipeline = hydrateAlphaSlug () (AlphaSlugRoute.BySlug(AlphaSlug ""))
        let! result = pipeline ctx
        test <@ result = Ok(AlphaSlugRoute.BySlug(AlphaSlug "helloworld")) @>
    }

[<Fact>]
let ``constrained parser returns error for missing value`` () =
    task {
        let ctx = createMockContextWithRoute []
        let pipeline = hydrateAlphaSlug () (AlphaSlugRoute.BySlug(AlphaSlug ""))
        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

// =============================================================================
// Returns<'T> hydration tests
// =============================================================================

type Fortune = { Message: string }

type ReturnsRoute =
    | List of Returns<Fortune list>
    | Show of id: Guid * Returns<Fortune>
    | WithAuth of PreCondition<UserId> * Returns<Fortune>

let hydrateReturns () =
    Route.extractor<ReturnsRoute, TestError> [ userPrecondition () ] [] makeError combineErrors

[<Fact>]
let ``Returns field gets default value during hydration`` () =
    task {
        let ctx = createMockContextWithRoute []
        let pipeline = hydrateReturns () (ReturnsRoute.List(Returns()))
        let! result = pipeline ctx
        test <@ result = Ok(ReturnsRoute.List(Returns())) @>
    }

[<Fact>]
let ``Returns field coexists with path param during hydration`` () =
    task {
        let id = Guid.NewGuid()
        let ctx = createMockContextWithRoute [ ("id", id.ToString()) ]
        let pipeline = hydrateReturns () (ReturnsRoute.Show(Guid.Empty, Returns()))
        let! result = pipeline ctx
        test <@ result = Ok(ReturnsRoute.Show(id, Returns())) @>
    }

[<Fact>]
let ``Returns field coexists with PreCondition during hydration`` () =
    task {
        let userId = Guid.NewGuid()
        let ctx = createMockContextWithRoute []
        ctx.Request.Headers.Append("X-User-Id", userId.ToString())

        let pipeline =
            hydrateReturns () (ReturnsRoute.WithAuth(PreCondition(UserId Guid.Empty), Returns()))

        let! result = pipeline ctx
        Assert.Equal(Ok(ReturnsRoute.WithAuth(PreCondition(UserId userId), Returns())), result)
    }

[<Fact>]
let ``Returns field does not interfere with precondition errors`` () =
    task {
        let ctx = createMockContextWithRoute []

        let pipeline =
            hydrateReturns () (ReturnsRoute.WithAuth(PreCondition(UserId Guid.Empty), Returns()))

        let! result = pipeline ctx
        Assert.Equal(Error NotAuthenticated, result)
    }

// =============================================================================
// JsonBody<'T> / FormBody<'T> hydration tests
// =============================================================================

type PostInput = { Title: string; Body: string }

type LoginInput = { Username: string; Password: string }

type JsonBodyRoute =
    | Create of JsonBody<PostInput>
    | CreateWithAuth of JsonBody<PostInput> * PreCondition<UserId>
    | CreateWithIdAndBody of id: Guid * JsonBody<PostInput>
    | CreateWithReturns of JsonBody<PostInput> * Returns<PostInput>

type FormBodyRoute =
    | Submit of FormBody<LoginInput>
    | SubmitWithAuth of FormBody<LoginInput> * PreCondition<UserId>

let createMockContextWithJsonBody (json: string) =
    let context = DefaultHttpContext()
    let bytes = System.Text.Encoding.UTF8.GetBytes(json)
    context.Request.Body <- new System.IO.MemoryStream(bytes)
    context.Request.ContentType <- "application/json"
    context :> HttpContext

let createMockContextWithJsonBodyAndRoute (json: string) (routeValues: (string * string) list) =
    let context = DefaultHttpContext()
    let bytes = System.Text.Encoding.UTF8.GetBytes(json)
    context.Request.Body <- new System.IO.MemoryStream(bytes)
    context.Request.ContentType <- "application/json"

    for (key, value) in routeValues do
        context.Request.RouteValues.Add(key, value)

    context :> HttpContext

let createMockContextWithFormBody (fields: (string * string) list) =
    let context = DefaultHttpContext()
    let dict = Dictionary<string, StringValues>()

    for (k, v) in fields do
        dict.[k] <- StringValues(v)

    context.Request.ContentType <- "application/x-www-form-urlencoded"
    context.Request.Form <- FormCollection(dict)
    context :> HttpContext

let hydrateJsonBody () =
    Route.extractor<JsonBodyRoute, TestError> [ userPrecondition () ] [] makeError combineErrors

let hydrateFormBody () =
    Route.extractor<FormBodyRoute, TestError> [ userPrecondition () ] [] makeError combineErrors

[<Fact>]
let ``JsonBody deserializes record from body`` () =
    task {
        let json = """{"Title":"Hello","Body":"World"}"""
        let ctx = createMockContextWithJsonBody json

        let pipeline =
            hydrateJsonBody () (JsonBodyRoute.Create(JsonBody { Title = ""; Body = "" }))

        let! result = pipeline ctx
        test <@ result = Ok(JsonBodyRoute.Create(JsonBody { Title = "Hello"; Body = "World" })) @>
    }

[<Fact>]
let ``JsonBody coexists with path params`` () =
    task {
        let id = Guid.NewGuid()
        let json = """{"Title":"Test","Body":"Content"}"""
        let ctx = createMockContextWithJsonBodyAndRoute json [ ("id", id.ToString()) ]

        let pipeline =
            hydrateJsonBody () (JsonBodyRoute.CreateWithIdAndBody(Guid.Empty, JsonBody { Title = ""; Body = "" }))

        let! result = pipeline ctx
        test <@ result = Ok(JsonBodyRoute.CreateWithIdAndBody(id, JsonBody { Title = "Test"; Body = "Content" })) @>
    }

[<Fact>]
let ``JsonBody coexists with PreCondition`` () =
    task {
        let userId = Guid.NewGuid()
        let json = """{"Title":"Auth","Body":"Test"}"""
        let ctx = createMockContextWithJsonBody json
        ctx.Request.Headers.Append("X-User-Id", userId.ToString())

        let pipeline =
            hydrateJsonBody
                ()
                (JsonBodyRoute.CreateWithAuth(JsonBody { Title = ""; Body = "" }, PreCondition(UserId Guid.Empty)))

        let! result = pipeline ctx

        Assert.Equal(
            Ok(JsonBodyRoute.CreateWithAuth(JsonBody { Title = "Auth"; Body = "Test" }, PreCondition(UserId userId))),
            result
        )
    }

[<Fact>]
let ``JsonBody coexists with Returns`` () =
    task {
        let json = """{"Title":"Ret","Body":"Test"}"""
        let ctx = createMockContextWithJsonBody json

        let pipeline =
            hydrateJsonBody () (JsonBodyRoute.CreateWithReturns(JsonBody { Title = ""; Body = "" }, Returns()))

        let! result = pipeline ctx

        test <@ result = Ok(JsonBodyRoute.CreateWithReturns(JsonBody { Title = "Ret"; Body = "Test" }, Returns())) @>
    }

[<Fact>]
let ``JsonBody returns error for invalid JSON`` () =
    task {
        let json = """not valid json"""
        let ctx = createMockContextWithJsonBody json

        let pipeline =
            hydrateJsonBody () (JsonBodyRoute.Create(JsonBody { Title = ""; Body = "" }))

        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

[<Fact>]
let ``JsonBody returns error for empty body`` () =
    task {
        let json = ""
        let ctx = createMockContextWithJsonBody json

        let pipeline =
            hydrateJsonBody () (JsonBodyRoute.Create(JsonBody { Title = ""; Body = "" }))

        let! result = pipeline ctx
        test <@ Result.isError result @>
    }

[<Fact>]
let ``FormBody extracts record from form`` () =
    task {
        let ctx =
            createMockContextWithFormBody [ ("Username", "admin"); ("Password", "secret") ]

        let pipeline =
            hydrateFormBody () (FormBodyRoute.Submit(FormBody { Username = ""; Password = "" }))

        let! result = pipeline ctx

        test
            <@
                result = Ok(
                    FormBodyRoute.Submit(
                        FormBody
                            { Username = "admin"
                              Password = "secret" }
                    )
                )
            @>
    }

[<Fact>]
let ``FormBody coexists with PreCondition`` () =
    task {
        let userId = Guid.NewGuid()

        let ctx =
            createMockContextWithFormBody [ ("Username", "admin"); ("Password", "secret") ]

        ctx.Request.Headers.Append("X-User-Id", userId.ToString())

        let pipeline =
            hydrateFormBody
                ()
                (FormBodyRoute.SubmitWithAuth(
                    FormBody { Username = ""; Password = "" },
                    PreCondition(UserId Guid.Empty)
                ))

        let! result = pipeline ctx

        Assert.Equal(
            Ok(
                FormBodyRoute.SubmitWithAuth(
                    FormBody
                        { Username = "admin"
                          Password = "secret" },
                    PreCondition(UserId userId)
                )
            ),
            result
        )
    }

[<Fact>]
let ``FormBody returns error for missing form`` () =
    task {
        let ctx = DefaultHttpContext() :> HttpContext

        let pipeline =
            hydrateFormBody () (FormBodyRoute.Submit(FormBody { Username = ""; Password = "" }))

        let! result = pipeline ctx
        test <@ Result.isError result @>
    }
