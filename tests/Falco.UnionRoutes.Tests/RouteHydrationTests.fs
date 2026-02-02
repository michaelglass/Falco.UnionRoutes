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

type TestError =
    | NotAuthenticated
    | BadRequest of string

type SimpleRoute =
    | List
    | Detail of id: Guid
    | WithAuth of UserId
    | WithBoth of UserId * id: Guid

type StringRoute = BySlug of slug: string

type IntRoute = ByPage of page: int

// Wrapper types (single-case DUs)
type PostId = PostId of Guid

type WrapperRoute =
    | ByPostId of id: PostId
    | WithAuthAndPostId of UserId * id: PostId

// Custom type for testing custom extractors
type Slug = Slug of string

type CustomTypeRoute =
    | BySlugCustom of slug: Slug
    | WithAuthAndSlug of UserId * slug: Slug

// Route for testing createNoAuth
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

let mockAuth () : Pipeline<UserId, TestError> =
    fun ctx ->
        match ctx.Request.Headers.TryGetValue("X-User-Id") with
        | true, values ->
            match Guid.TryParse(values.ToString()) with
            | true, guid -> Ok(UserId guid)
            | false, _ -> Error NotAuthenticated
        | false, _ -> Error NotAuthenticated

/// Convert extraction error string to TestError
let makeError (msg: string) = BadRequest msg

/// Combine multiple errors - preserves structure for pattern matching
let combineErrors (errors: TestError list) =
    match errors with
    | [ single ] -> single
    | multiple -> BadRequest(multiple |> List.map string |> String.concat "; ")

let hydrate () =
    RouteHydration.create<SimpleRoute, UserId, TestError> (mockAuth ()) makeError combineErrors

let hydrateString () =
    RouteHydration.create<StringRoute, UserId, TestError> (mockAuth ()) makeError combineErrors

let hydrateInt () =
    RouteHydration.create<IntRoute, UserId, TestError> (mockAuth ()) makeError combineErrors

let hydrateWrapper () =
    RouteHydration.create<WrapperRoute, UserId, TestError> (mockAuth ()) makeError combineErrors

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
    RouteHydration.createWith<CustomTypeRoute, UserId, TestError>
        [ slugExtractor ]
        (mockAuth ())
        makeError
        combineErrors

let hydrateNoAuth () =
    RouteHydration.createNoAuth<NoAuthRoute, TestError> makeError combineErrors

let hydrateNoAuthWithCustom () =
    RouteHydration.createNoAuthWith<CustomTypeRoute, TestError> [ slugExtractor ] makeError combineErrors

let hydrateQuery () =
    RouteHydration.createNoAuth<QueryRoute, TestError> makeError combineErrors

let hydrateMultiField () =
    RouteHydration.createNoAuth<MultiFieldRoute, TestError> makeError combineErrors

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

// =============================================================================
// Auth field tests
// =============================================================================

[<Fact>]
let ``hydrates auth field from pipeline`` () =
    let userId = Guid.NewGuid()
    let ctx = createMockContextWithRoute []
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())
    let pipeline = hydrate () (SimpleRoute.WithAuth(UserId Guid.Empty))
    test <@ pipeline ctx = Ok(SimpleRoute.WithAuth(UserId userId)) @>

[<Fact>]
let ``returns auth error when not authenticated`` () =
    let ctx = createMockContextWithRoute []
    let pipeline = hydrate () (SimpleRoute.WithAuth(UserId Guid.Empty))
    // Auth errors preserve their original type
    test <@ pipeline ctx = Error NotAuthenticated @>

// =============================================================================
// Combined field tests
// =============================================================================

[<Fact>]
let ``hydrates both auth and Guid fields`` () =
    let userId = Guid.NewGuid()
    let postId = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", postId.ToString()) ]
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())
    let pipeline = hydrate () (SimpleRoute.WithBoth(UserId Guid.Empty, Guid.Empty))
    test <@ pipeline ctx = Ok(SimpleRoute.WithBoth(UserId userId, postId)) @>

[<Fact>]
let ``returns auth error even with valid Guid when not authenticated`` () =
    let postId = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", postId.ToString()) ]
    let pipeline = hydrate () (SimpleRoute.WithBoth(UserId Guid.Empty, Guid.Empty))
    // Auth errors preserve their original type
    test <@ pipeline ctx = Error NotAuthenticated @>

[<Fact>]
let ``returns Guid error with valid auth but invalid Guid`` () =
    let userId = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", "invalid") ]
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())
    let pipeline = hydrate () (SimpleRoute.WithBoth(UserId Guid.Empty, Guid.Empty))
    let result = pipeline ctx
    test <@ Result.isError result @>

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

// =============================================================================
// Wrapper type tests (single-case DUs like PostId)
// =============================================================================

[<Fact>]
let ``hydrates wrapper type field from route params`` () =
    let id = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", id.ToString()) ]
    let pipeline = hydrateWrapper () (WrapperRoute.ByPostId(PostId Guid.Empty))
    test <@ pipeline ctx = Ok(WrapperRoute.ByPostId(PostId id)) @>

[<Fact>]
let ``returns error for invalid Guid in wrapper type`` () =
    let ctx = createMockContextWithRoute [ ("id", "not-a-guid") ]
    let pipeline = hydrateWrapper () (WrapperRoute.ByPostId(PostId Guid.Empty))
    let result = pipeline ctx
    test <@ Result.isError result @>

[<Fact>]
let ``hydrates both auth and wrapper type fields`` () =
    let userId = Guid.NewGuid()
    let postId = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", postId.ToString()) ]
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())

    let pipeline =
        hydrateWrapper () (WrapperRoute.WithAuthAndPostId(UserId Guid.Empty, PostId Guid.Empty))

    test <@ pipeline ctx = Ok(WrapperRoute.WithAuthAndPostId(UserId userId, PostId postId)) @>

// =============================================================================
// Custom extractor tests
// =============================================================================

[<Fact>]
let ``custom extractor handles custom type`` () =
    let ctx = createMockContextWithRoute [ ("slug", "hello-world") ]
    let pipeline = hydrateCustom () (CustomTypeRoute.BySlugCustom(Slug ""))
    test <@ pipeline ctx = Ok(CustomTypeRoute.BySlugCustom(Slug "hello-world")) @>

[<Fact>]
let ``custom extractor returns error for missing value`` () =
    let ctx = createMockContextWithRoute []
    let pipeline = hydrateCustom () (CustomTypeRoute.BySlugCustom(Slug ""))
    let result = pipeline ctx
    test <@ Result.isError result @>

[<Fact>]
let ``custom extractor works with auth`` () =
    let userId = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("slug", "my-post") ]
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())

    let pipeline =
        hydrateCustom () (CustomTypeRoute.WithAuthAndSlug(UserId Guid.Empty, Slug ""))

    test <@ pipeline ctx = Ok(CustomTypeRoute.WithAuthAndSlug(UserId userId, Slug "my-post")) @>

// =============================================================================
// createNoAuth tests
// =============================================================================

[<Fact>]
let ``createNoAuth hydrates Guid field`` () =
    let id = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", id.ToString()) ]
    let pipeline = hydrateNoAuth () (NoAuthRoute.SimpleNoAuth Guid.Empty)
    test <@ pipeline ctx = Ok(NoAuthRoute.SimpleNoAuth id) @>

[<Fact>]
let ``createNoAuth hydrates wrapper type`` () =
    let id = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", id.ToString()) ]
    let pipeline = hydrateNoAuth () (NoAuthRoute.WithWrapper(PostId Guid.Empty))
    test <@ pipeline ctx = Ok(NoAuthRoute.WithWrapper(PostId id)) @>

[<Fact>]
let ``createNoAuthWith uses custom extractors`` () =
    let ctx = createMockContextWithRoute [ ("slug", "custom-slug") ]
    let pipeline = hydrateNoAuthWithCustom () (CustomTypeRoute.BySlugCustom(Slug ""))
    test <@ pipeline ctx = Ok(CustomTypeRoute.BySlugCustom(Slug "custom-slug")) @>

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
// Error accumulation tests (now the default behavior)
// =============================================================================

[<Fact>]
let ``hydration collects all errors`` () =
    let ctx = createMockContextWithRoute []
    let pipeline = hydrateMultiField () (MultiFieldRoute.TwoRequired(Guid.Empty, 0))
    let result = pipeline ctx

    match result with
    | Error(BadRequest msg) -> test <@ msg.Contains("a") && msg.Contains("b") @>
    | _ -> test <@ false @>

[<Fact>]
let ``hydration succeeds when all fields valid`` () =
    let id = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("a", id.ToString()); ("b", "42") ]
    let pipeline = hydrateMultiField () (MultiFieldRoute.TwoRequired(Guid.Empty, 0))
    test <@ pipeline ctx = Ok(MultiFieldRoute.TwoRequired(id, 42)) @>
