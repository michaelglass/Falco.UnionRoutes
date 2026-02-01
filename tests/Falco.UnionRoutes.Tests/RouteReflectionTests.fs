module Falco.UnionRoutes.Tests.RouteReflectionTests

open System
open Xunit
open Swensen.Unquote
open Falco.UnionRoutes

// =============================================================================
// Test route definitions
// =============================================================================

type PostRoute =
    | [<Route(RouteMethod.Get, Path = "posts")>] List
    | [<Route(RouteMethod.Get, Path = "posts/{id}")>] Detail of id: Guid
    | [<Route(RouteMethod.Post, Path = "posts")>] Create
    | [<Route(RouteMethod.Put, Path = "posts/{id}")>] Update of id: Guid
    | [<Route(RouteMethod.Delete, Path = "posts/{id}")>] Delete of id: Guid

type UserRoute =
    | [<Route(RouteMethod.Get, Path = "users")>] List
    | [<Route(RouteMethod.Get, Path = "users/{id}")>] Profile of id: Guid

type ApiRoute =
    | [<Route(RouteMethod.Get, Path = "")>] Posts of PostRoute
    | [<Route(RouteMethod.Get, Path = "")>] Users of UserRoute

type TestRoute =
    | [<Route(RouteMethod.Get, Path = "")>] Api of ApiRoute
    | [<Route(RouteMethod.Get, Path = "health")>] Health
    | [<Route(RouteMethod.Get, Path = "")>] Home

// =============================================================================
// toKebabCase tests
// =============================================================================

[<Fact>]
let ``toKebabCase converts PascalCase to kebab-case`` () =
    test <@ RouteReflection.toKebabCase "DigestView" = "digest-view" @>
    test <@ RouteReflection.toKebabCase "LoginPage" = "login-page" @>
    test <@ RouteReflection.toKebabCase "Dashboard" = "dashboard" @>

[<Fact>]
let ``toKebabCase handles single word`` () =
    test <@ RouteReflection.toKebabCase "Home" = "home" @>
    test <@ RouteReflection.toKebabCase "Profile" = "profile" @>

[<Fact>]
let ``toKebabCase handles consecutive capitals correctly`` () =
    // Consecutive capitals get split before final lowercase transition
    test <@ RouteReflection.toKebabCase "HTMLParser" = "html-parser" @>
    test <@ RouteReflection.toKebabCase "URLHandler" = "url-handler" @>
    test <@ RouteReflection.toKebabCase "GetAPIData" = "get-api-data" @>

// =============================================================================
// Simple route tests
// =============================================================================

[<Fact>]
let ``Health route returns correct path`` () =
    let info = RouteReflection.routeInfo TestRoute.Health
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/health" @>

[<Fact>]
let ``Home route with empty path returns root`` () =
    let info = RouteReflection.routeInfo TestRoute.Home
    test <@ info.Path = "/" @>

// =============================================================================
// Parameterized route tests
// =============================================================================

[<Fact>]
let ``PostRoute.List returns correct path`` () =
    let info = RouteReflection.routeInfo PostRoute.List
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/posts" @>

[<Fact>]
let ``PostRoute.Detail includes parameter placeholder`` () =
    let id = Guid.NewGuid()
    let info = RouteReflection.routeInfo (PostRoute.Detail id)
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/posts/{id}" @>

[<Fact>]
let ``PostRoute.Create uses POST method`` () =
    let info = RouteReflection.routeInfo PostRoute.Create
    test <@ info.Method = HttpMethod.Post @>

[<Fact>]
let ``PostRoute.Update uses PUT method`` () =
    let info = RouteReflection.routeInfo (PostRoute.Update(Guid.NewGuid()))
    test <@ info.Method = HttpMethod.Put @>

[<Fact>]
let ``PostRoute.Delete uses DELETE method`` () =
    let info = RouteReflection.routeInfo (PostRoute.Delete(Guid.NewGuid()))
    test <@ info.Method = HttpMethod.Delete @>

// =============================================================================
// Nested route tests
// =============================================================================

[<Fact>]
let ``Nested route combines paths correctly`` () =
    let info = RouteReflection.routeInfo (TestRoute.Api(ApiRoute.Posts PostRoute.List))
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/posts" @>

[<Fact>]
let ``Deeply nested route with param`` () =
    let id = Guid.NewGuid()

    let info =
        RouteReflection.routeInfo (TestRoute.Api(ApiRoute.Posts(PostRoute.Detail id)))

    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/posts/{id}" @>

[<Fact>]
let ``Nested route inherits method from leaf`` () =
    let info =
        RouteReflection.routeInfo (TestRoute.Api(ApiRoute.Posts PostRoute.Create))

    test <@ info.Method = HttpMethod.Post @>

// =============================================================================
// routeInfo (throwing version) tests
// =============================================================================

[<Fact>]
let ``routeInfo returns info for valid route`` () =
    let info = RouteReflection.routeInfo PostRoute.List
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/posts" @>

[<Fact>]
let ``routeTuple returns method and path`` () =
    let (method, path) = RouteReflection.routeTuple PostRoute.List
    test <@ method = HttpMethod.Get @>
    test <@ path = "/posts" @>

// =============================================================================
// allRoutes enumeration tests
// =============================================================================

[<Fact>]
let ``allRoutes enumerates all route cases`` () =
    let routes = RouteReflection.allRoutes<PostRoute> ()
    test <@ List.length routes = 5 @>

[<Fact>]
let ``allRoutes enumerates nested routes`` () =
    let routes = RouteReflection.allRoutes<TestRoute> ()
    // TestRoute has: Api (containing Posts + Users), Health, Home
    // Posts: 5 cases, Users: 2 cases = 7 from Api + 2 top-level = 9 total
    test <@ List.length routes = 9 @>

[<Fact>]
let ``allRoutes uses default values for parameters`` () =
    let routes = RouteReflection.allRoutes<PostRoute> ()

    let detailRoute =
        routes
        |> List.find (fun r ->
            match r with
            | PostRoute.Detail _ -> true
            | _ -> false)

    match detailRoute with
    | PostRoute.Detail id -> test <@ id = Guid.Empty @>
    | _ -> failwith "Expected Detail route"

[<Fact>]
let ``All enumerated routes have valid RouteAttribute`` () =
    let routes = RouteReflection.allRoutes<TestRoute> ()

    let invalidRoutes =
        routes
        |> List.choose (fun route ->
            match RouteReflection.tryRouteInfo route with
            | None -> Some $"Route missing attribute: {route}"
            | Some _ -> None)

    test <@ List.isEmpty invalidRoutes @>
