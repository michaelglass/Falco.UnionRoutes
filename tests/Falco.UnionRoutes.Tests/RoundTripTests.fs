module Falco.UnionRoutes.Tests.RoundTripTests

open System
open Xunit
open Swensen.Unquote
open Falco.UnionRoutes

// =============================================================================
// Round-trip property: matchUrl method (link r) = Ok r
//
// This is the library's core invariant and the arbiter for percent-encoding of
// substituted link values. A representative route DU covers ints, int64, bools,
// GUIDs, strings (including special characters), single-case wrappers, and
// nested unions.
// =============================================================================

type ItemId = ItemId of Guid

type ChildRoute =
    | [<Route(RouteMethod.Get, Path = "")>] Show
    | [<Route(RouteMethod.Get, Path = "edit")>] Edit

type RtRoute =
    | [<Route(RouteMethod.Get, Path = "health")>] Health
    | [<Route(RouteMethod.Get, Path = "items/{id}")>] IntItem of id: int
    | [<Route(RouteMethod.Get, Path = "longs/{id}")>] LongItem of id: int64
    | [<Route(RouteMethod.Get, Path = "flags/{enabled}")>] Flag of enabled: bool
    | [<Route(RouteMethod.Get, Path = "guids/{id}")>] GuidItem of id: Guid
    | [<Route(RouteMethod.Get, Path = "slugs/{slug}")>] SlugItem of slug: string
    | [<Route(RouteMethod.Get, Path = "wrapped/{id}")>] WrappedItem of id: ItemId
    | [<Route(RouteMethod.Get, Path = "nested/{id}")>] Nested of id: Guid * ChildRoute

let private testGuid = Guid.Parse("550e8400-e29b-41d4-a716-446655440000")

/// Representative route values whose path fields are fully reconstructible from the URL.
let roundTripCases: obj[] list =
    [ [| box Health |]
      [| box (IntItem 42) |]
      [| box (IntItem -7) |]
      [| box (LongItem 9999999999L) |]
      [| box (Flag true) |]
      [| box (Flag false) |]
      [| box (GuidItem testGuid) |]
      [| box (SlugItem "hello-world") |]
      // Special characters that MUST be percent-encoded to survive the round trip.
      [| box (SlugItem "hello world") |]
      [| box (SlugItem "a/b") |]
      [| box (SlugItem "x?y#z") |]
      [| box (SlugItem "100%") |]
      [| box (SlugItem "a&b=c") |]
      [| box (WrappedItem(ItemId testGuid)) |]
      [| box (Nested(testGuid, Show)) |]
      [| box (Nested(testGuid, Edit)) |] ]

[<Theory>]
[<MemberData(nameof roundTripCases)>]
let ``matchUrl (link r) round-trips to Ok r`` (route: RtRoute) =
    let url = Route.link route
    let info = Route.info route
    let matched = Route.matchUrl<RtRoute> info.Method url
    test <@ matched = Ok route @>

[<Fact>]
let ``link percent-encodes a space`` () =
    let url = Route.link (SlugItem "hello world")
    test <@ url = "/slugs/hello%20world" @>

[<Fact>]
let ``link percent-encodes a slash`` () =
    let url = Route.link (SlugItem "a/b")
    test <@ url = "/slugs/a%2Fb" @>

[<Fact>]
let ``link percent-encodes query and fragment delimiters`` () =
    let url = Route.link (SlugItem "x?y#z")
    test <@ url = "/slugs/x%3Fy%23z" @>
