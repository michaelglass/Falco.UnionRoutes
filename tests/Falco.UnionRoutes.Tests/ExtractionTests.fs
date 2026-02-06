module Falco.UnionRoutes.Tests.ExtractionTests

open System
open Xunit
open Swensen.Unquote
open Falco.UnionRoutes
open Falco.UnionRoutes.Tests.TestHelpers
open Microsoft.AspNetCore.Http

// =============================================================================
// Test types
// =============================================================================

type UserId = UserId of Guid
type PostId = PostId of Guid

type TestError =
    | NotAuthenticated
    | NotFound of string
    | BadRequest of string

// =============================================================================
// Mock HttpContext helpers
// =============================================================================

let createMockContext () =
    let context = DefaultHttpContext()
    context :> HttpContext

let createMockContextWithRoute (routeValues: (string * string) list) =
    let context = DefaultHttpContext()

    for (key, value) in routeValues do
        context.Request.RouteValues.Add(key, value)

    context :> HttpContext

// =============================================================================
// requireSome tests
// =============================================================================

[<Fact>]
let ``requireSome returns Ok for Some value`` () =
    test <@ requireSome TestError.NotAuthenticated (Some 42) = Ok 42 @>

[<Fact>]
let ``requireSome returns Error for None`` () =
    test <@ requireSome TestError.NotAuthenticated None = Error TestError.NotAuthenticated @>

[<Fact>]
let ``requireSomeWith returns Ok for Some value`` () =
    test <@ requireSomeWith (fun () -> TestError.NotAuthenticated) (Some "hello") = Ok "hello" @>

[<Fact>]
let ``requireSomeWith returns Error with lazy evaluation for None`` () =
    let mutable called = false

    let errorFn () =
        called <- true
        TestError.NotAuthenticated

    requireSomeWith errorFn None |> ignore
    test <@ called @>

// =============================================================================
// Extractor composition tests
// =============================================================================

[<Fact>]
let ``Extractor composition with <&> combines results`` () =
    let p1: Extractor<int, TestError> = fun _ -> Ok 1
    let p2: Extractor<string, TestError> = fun _ -> Ok "hello"
    let combined = p1 <&> p2
    let ctx = createMockContext ()
    test <@ combined ctx = Ok(1, "hello") @>

[<Fact>]
let ``Extractor composition short-circuits on first error`` () =
    let p1: Extractor<int, TestError> = fun _ -> Error NotAuthenticated
    let p2: Extractor<string, TestError> = fun _ -> Ok "hello"
    let combined = p1 <&> p2
    let ctx = createMockContext ()
    test <@ combined ctx = Error NotAuthenticated @>

[<Fact>]
let ``Extractor composition returns second error if first succeeds`` () =
    let p1: Extractor<int, TestError> = fun _ -> Ok 1
    let p2: Extractor<string, TestError> = fun _ -> Error(NotFound "not found")
    let combined = p1 <&> p2
    let ctx = createMockContext ()
    test <@ combined ctx = Error(NotFound "not found") @>

[<Fact>]
let ``Triple composition works`` () =
    let p1: Extractor<int, TestError> = fun _ -> Ok 1
    let p2: Extractor<string, TestError> = fun _ -> Ok "two"
    let p3: Extractor<bool, TestError> = fun _ -> Ok true
    let combined = p1 <&> p2 <&> p3
    let ctx = createMockContext ()
    test <@ combined ctx = Ok((1, "two"), true) @>

// =============================================================================
// Route parameter extraction tests
// =============================================================================

[<Fact>]
let ``tryGetRouteGuid returns Some for valid GUID`` () =
    let guid = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", guid.ToString()) ]
    test <@ tryGetRouteGuid ctx "id" = Some guid @>

[<Fact>]
let ``tryGetRouteGuid returns None for invalid GUID`` () =
    let ctx = createMockContextWithRoute [ ("id", "not-a-guid") ]
    test <@ tryGetRouteGuid ctx "id" = None @>

[<Fact>]
let ``tryGetRouteGuid returns None for missing parameter`` () =
    let ctx = createMockContext ()
    test <@ tryGetRouteGuid ctx "id" = None @>

[<Fact>]
let ``requireRouteId returns typed ID for valid GUID`` () =
    let guid = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", guid.ToString()) ]
    let pipeline = requireRouteId "id" UserId (BadRequest "Invalid ID")
    test <@ pipeline ctx = Ok(UserId guid) @>

[<Fact>]
let ``requireRouteId returns error for invalid GUID`` () =
    let ctx = createMockContextWithRoute [ ("id", "invalid") ]
    let pipeline = requireRouteId "id" UserId (BadRequest "Invalid ID")
    test <@ pipeline ctx = Error(BadRequest "Invalid ID") @>

[<Fact>]
let ``requireRouteStr returns string for valid param`` () =
    let ctx = createMockContextWithRoute [ ("name", "test-value") ]
    let pipeline = requireRouteStr "name" (BadRequest "Missing name")
    test <@ pipeline ctx = Ok "test-value" @>

[<Fact>]
let ``requireRouteStr returns error for empty param`` () =
    let ctx = createMockContextWithRoute [ ("name", "") ]
    let pipeline = requireRouteStr "name" (BadRequest "Missing name")
    test <@ pipeline ctx = Error(BadRequest "Missing name") @>

[<Fact>]
let ``requireRouteInt returns int for valid number`` () =
    let ctx = createMockContextWithRoute [ ("page", "42") ]
    let pipeline = requireRouteInt "page" (BadRequest "Invalid page")
    test <@ pipeline ctx = Ok 42 @>

[<Fact>]
let ``requireRouteInt returns error for non-numeric`` () =
    let ctx = createMockContextWithRoute [ ("page", "abc") ]
    let pipeline = requireRouteInt "page" (BadRequest "Invalid page")
    test <@ pipeline ctx = Error(BadRequest "Invalid page") @>

[<Fact>]
let ``requireRouteIntWith returns int for valid number`` () =
    let ctx = createMockContextWithRoute [ ("page", "123") ]
    let pipeline = requireRouteIntWith "page" (fun () -> BadRequest "Invalid page")
    test <@ pipeline ctx = Ok 123 @>

[<Fact>]
let ``requireRouteIntWith uses lazy error for invalid number`` () =
    let mutable called = false

    let errorFn () =
        called <- true
        BadRequest "Invalid page"

    let ctx = createMockContextWithRoute [ ("page", "not-a-number") ]
    let pipeline = requireRouteIntWith "page" errorFn
    pipeline ctx |> ignore
    test <@ called @>

// =============================================================================
// ignoreResult tests
// =============================================================================

[<Fact>]
let ``ignoreResult converts success to unit`` () =
    let p: Extractor<int, TestError> = fun _ -> Ok 42
    let ignored = ignoreResult p
    let ctx = createMockContext ()
    test <@ ignored ctx = Ok() @>

[<Fact>]
let ``ignoreResult preserves error`` () =
    let p: Extractor<int, TestError> = fun _ -> Error NotAuthenticated
    let ignored = ignoreResult p
    let ctx = createMockContext ()
    test <@ ignored ctx = Error NotAuthenticated @>

// =============================================================================
// Real-world composition tests
// =============================================================================

[<Fact>]
let ``Real-world: auth + route param composition`` () =
    let requireAuth: Extractor<UserId, TestError> =
        fun ctx ->
            match ctx.Request.Headers.TryGetValue("X-User-Id") with
            | true, values ->
                match Guid.TryParse(values.ToString()) with
                | true, guid -> Ok(UserId guid)
                | false, _ -> Error NotAuthenticated
            | false, _ -> Error NotAuthenticated

    let requirePostId = requireRouteId "id" PostId (BadRequest "Invalid post ID")
    let combined = requireAuth <&> requirePostId

    let userId = Guid.NewGuid()
    let postId = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", postId.ToString()) ]
    ctx.Request.Headers.Append("X-User-Id", userId.ToString())

    test <@ combined ctx = Ok(UserId userId, PostId postId) @>

[<Fact>]
let ``Real-world: auth failure short-circuits`` () =
    let requireAuth: Extractor<UserId, TestError> = fun _ -> Error NotAuthenticated
    let requirePostId = requireRouteId "id" PostId (BadRequest "Invalid post ID")
    let combined = requireAuth <&> requirePostId

    let postId = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", postId.ToString()) ]

    test <@ combined ctx = Error NotAuthenticated @>
