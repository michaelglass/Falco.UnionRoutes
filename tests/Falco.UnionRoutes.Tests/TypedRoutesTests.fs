module Falco.UnionRoutes.Tests.TypedRoutesTests

open System
open Xunit
open Swensen.Unquote
open Falco.UnionRoutes
open Microsoft.AspNetCore.Http

// =============================================================================
// Test types - demonstrating the Servant-inspired approach
// =============================================================================

/// Route definition - the union case fields define the parameter types
type PostRoute =
    | List // No params: unit -> HttpHandler
    | Detail of id: Guid // One Guid param: Guid -> HttpHandler
    | BySlug of slug: string // One string param: string -> HttpHandler
    | Page of page: int // One int param: int -> HttpHandler

type TestError =
    | BadRequest of string
    | NotFound

// =============================================================================
// Mock helpers
// =============================================================================

let createMockContextWithRoute (routeValues: (string * string) list) =
    let context = DefaultHttpContext()

    for (key, value) in routeValues do
        context.Request.RouteValues.Add(key, value)

    context :> HttpContext

// =============================================================================
// Tests for extractors (the core type-safe building blocks)
// =============================================================================

[<Fact>]
let ``extractGuid succeeds for valid GUID`` () =
    let id = Guid.NewGuid()
    let ctx = createMockContextWithRoute [ ("id", id.ToString()) ]
    test <@ extractGuid "id" (BadRequest "Invalid") ctx = Ok id @>

[<Fact>]
let ``extractGuid fails for invalid GUID`` () =
    let ctx = createMockContextWithRoute [ ("id", "not-a-guid") ]
    test <@ extractGuid "id" (BadRequest "Invalid") ctx = Error(BadRequest "Invalid") @>

[<Fact>]
let ``extractGuid fails for missing parameter`` () =
    let ctx = createMockContextWithRoute []
    test <@ extractGuid "id" (BadRequest "Missing") ctx = Error(BadRequest "Missing") @>

[<Fact>]
let ``extractString succeeds for non-empty string`` () =
    let ctx = createMockContextWithRoute [ ("slug", "hello-world") ]
    test <@ extractString "slug" (BadRequest "Invalid") ctx = Ok "hello-world" @>

[<Fact>]
let ``extractString fails for empty string`` () =
    let ctx = createMockContextWithRoute [ ("slug", "") ]
    test <@ extractString "slug" (BadRequest "Empty") ctx = Error(BadRequest "Empty") @>

[<Fact>]
let ``extractInt succeeds for valid integer`` () =
    let ctx = createMockContextWithRoute [ ("page", "42") ]
    test <@ extractInt "page" (BadRequest "Invalid") ctx = Ok 42 @>

[<Fact>]
let ``extractInt fails for non-numeric string`` () =
    let ctx = createMockContextWithRoute [ ("page", "abc") ]
    test <@ extractInt "page" (BadRequest "Invalid") ctx = Error(BadRequest "Invalid") @>

[<Fact>]
let ``extractInt handles negative numbers`` () =
    let ctx = createMockContextWithRoute [ ("page", "-5") ]
    test <@ extractInt "page" (BadRequest "Invalid") ctx = Ok -5 @>

// =============================================================================
// Tests for builder pattern
// =============================================================================

[<Fact>]
let ``typed builder creates empty builder`` () =
    let toError (_: TestError) =
        fun _ -> System.Threading.Tasks.Task.CompletedTask

    let builder = typed<PostRoute, TestError> toError
    test <@ builder.UnitCases.Length = 0 @>
    test <@ builder.TypedCases.Length = 0 @>

[<Fact>]
let ``route0 adds unit case`` () =
    let toError (_: TestError) =
        fun _ -> System.Threading.Tasks.Task.CompletedTask

    let handler = fun _ -> System.Threading.Tasks.Task.CompletedTask

    let builder =
        typed<PostRoute, TestError> toError
        |> route0
            (function
            | List -> true
            | _ -> false)
            handler

    test <@ builder.UnitCases.Length = 1 @>

[<Fact>]
let ``routeGuid adds typed case`` () =
    let toError (_: TestError) =
        fun _ -> System.Threading.Tasks.Task.CompletedTask

    let handler (_: Guid) =
        fun _ -> System.Threading.Tasks.Task.CompletedTask

    let builder =
        typed<PostRoute, TestError> toError
        |> routeGuid
            (function
            | Detail id -> Some id
            | _ -> None)
            "id"
            (BadRequest "Invalid")
            handler

    test <@ builder.TypedCases.Length = 1 @>

[<Fact>]
let ``build creates handler that matches unit cases`` () =
    let toError (_: TestError) =
        fun _ -> System.Threading.Tasks.Task.CompletedTask

    let mutable called = false

    let handler =
        typed<PostRoute, TestError> toError
        |> route0
            (function
            | List -> true
            | _ -> false)
            (fun _ ->
                called <- true
                System.Threading.Tasks.Task.CompletedTask)
        |> build

    let ctx = createMockContextWithRoute []
    handler List ctx |> ignore

    test <@ called @>

// =============================================================================
// Demonstrating the type safety advantage
// =============================================================================

// The key insight: the pattern function's return type constrains the handler type.
//
// For example, this pattern returns Guid option:
//   (function Detail id -> Some id | _ -> None)
//
// So routeGuid requires the handler to accept Guid:
//   (fun (id: Guid) -> ...)
//
// If you try to use the wrong type, you get a compile error:
//
// COMPILE ERROR: handler expects Guid, not string
// |> routeGuid pattern "id" error (fun (slug: string) -> ...)
//
// COMPILE ERROR: pattern returns Guid option, but routeInt expects int option
// |> routeInt (fun r -> match r with Detail id -> Some id | _ -> None) ...
//
// This is the closest F# can get to Servant's type-level API derivation.
// The pattern serves as both matcher AND type witness.
