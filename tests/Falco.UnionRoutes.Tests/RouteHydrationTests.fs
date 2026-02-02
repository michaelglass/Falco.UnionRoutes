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

// =============================================================================
// Mock helpers
// =============================================================================

let createMockContextWithRoute (routeValues: (string * string) list) =
    let context = DefaultHttpContext()

    for (key, value) in routeValues do
        context.Request.RouteValues.Add(key, value)

    context :> HttpContext

let mockAuth () : Pipeline<UserId, TestError> =
    fun ctx ->
        match ctx.Request.Headers.TryGetValue("X-User-Id") with
        | true, values ->
            match Guid.TryParse(values.ToString()) with
            | true, guid -> Ok(UserId guid)
            | false, _ -> Error NotAuthenticated
        | false, _ -> Error NotAuthenticated

let hydrate () =
    RouteHydration.create<SimpleRoute, UserId, TestError> (mockAuth ()) BadRequest

let hydrateString () =
    RouteHydration.create<StringRoute, UserId, TestError> (mockAuth ()) BadRequest

let hydrateInt () =
    RouteHydration.create<IntRoute, UserId, TestError> (mockAuth ()) BadRequest

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
