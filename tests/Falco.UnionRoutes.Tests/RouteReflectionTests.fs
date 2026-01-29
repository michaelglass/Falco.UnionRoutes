module Falco.UnionRoutes.Tests.RouteReflectionTests

open System
open Xunit
open FsUnit.Xunit
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
    Assert.Equal("digest-view", RouteReflection.toKebabCase "DigestView")
    Assert.Equal("login-page", RouteReflection.toKebabCase "LoginPage")
    Assert.Equal("dashboard", RouteReflection.toKebabCase "Dashboard")

[<Fact>]
let ``toKebabCase handles single word`` () =
    Assert.Equal("home", RouteReflection.toKebabCase "Home")
    Assert.Equal("profile", RouteReflection.toKebabCase "Profile")

[<Fact>]
let ``toKebabCase handles consecutive capitals as single block`` () =
    // Consecutive capitals stay together (standard behavior)
    Assert.Equal("htmlparser", RouteReflection.toKebabCase "HTMLParser")
    Assert.Equal("urlhandler", RouteReflection.toKebabCase "URLHandler")

// =============================================================================
// Simple route tests
// =============================================================================

[<Fact>]
let ``Health route returns correct path`` () =
    let result = RouteReflection.tryRouteInfo TestRoute.Health
    Assert.True(result.IsSome)
    let info = result.Value
    Assert.Equal(HttpMethod.Get, info.Method)
    Assert.Equal("/health", info.Path)

[<Fact>]
let ``Home route with empty path returns root`` () =
    let result = RouteReflection.tryRouteInfo TestRoute.Home
    Assert.True(result.IsSome)
    let info = result.Value
    Assert.Equal("/", info.Path)

// =============================================================================
// Parameterized route tests
// =============================================================================

[<Fact>]
let ``PostRoute.List returns correct path`` () =
    let result = RouteReflection.tryRouteInfo PostRoute.List
    Assert.True(result.IsSome)
    let info = result.Value
    Assert.Equal(HttpMethod.Get, info.Method)
    Assert.Equal("/posts", info.Path)

[<Fact>]
let ``PostRoute.Detail includes parameter placeholder`` () =
    let id = Guid.NewGuid()
    let result = RouteReflection.tryRouteInfo (PostRoute.Detail id)
    Assert.True(result.IsSome)
    let info = result.Value
    Assert.Equal(HttpMethod.Get, info.Method)
    Assert.Equal("/posts/{id}", info.Path)

[<Fact>]
let ``PostRoute.Create uses POST method`` () =
    let result = RouteReflection.tryRouteInfo PostRoute.Create
    Assert.True(result.IsSome)
    let info = result.Value
    Assert.Equal(HttpMethod.Post, info.Method)

[<Fact>]
let ``PostRoute.Update uses PUT method`` () =
    let result = RouteReflection.tryRouteInfo (PostRoute.Update(Guid.NewGuid()))
    Assert.True(result.IsSome)
    let info = result.Value
    Assert.Equal(HttpMethod.Put, info.Method)

[<Fact>]
let ``PostRoute.Delete uses DELETE method`` () =
    let result = RouteReflection.tryRouteInfo (PostRoute.Delete(Guid.NewGuid()))
    Assert.True(result.IsSome)
    let info = result.Value
    Assert.Equal(HttpMethod.Delete, info.Method)

// =============================================================================
// Nested route tests
// =============================================================================

[<Fact>]
let ``Nested route combines paths correctly`` () =
    let result =
        RouteReflection.tryRouteInfo (TestRoute.Api(ApiRoute.Posts PostRoute.List))

    Assert.True(result.IsSome)
    let info = result.Value
    Assert.Equal(HttpMethod.Get, info.Method)
    Assert.Equal("/posts", info.Path)

[<Fact>]
let ``Deeply nested route with param`` () =
    let id = Guid.NewGuid()

    let result =
        RouteReflection.tryRouteInfo (TestRoute.Api(ApiRoute.Posts(PostRoute.Detail id)))

    Assert.True(result.IsSome)
    let info = result.Value
    Assert.Equal(HttpMethod.Get, info.Method)
    Assert.Equal("/posts/{id}", info.Path)

[<Fact>]
let ``Nested route inherits method from leaf`` () =
    let result =
        RouteReflection.tryRouteInfo (TestRoute.Api(ApiRoute.Posts PostRoute.Create))

    Assert.True(result.IsSome)
    let info = result.Value
    Assert.Equal(HttpMethod.Post, info.Method)

// =============================================================================
// routeInfo (throwing version) tests
// =============================================================================

[<Fact>]
let ``routeInfo returns info for valid route`` () =
    let info = RouteReflection.routeInfo PostRoute.List
    Assert.Equal(HttpMethod.Get, info.Method)
    Assert.Equal("/posts", info.Path)

[<Fact>]
let ``routeTuple returns method and path`` () =
    let (method, path) = RouteReflection.routeTuple PostRoute.List
    Assert.Equal(HttpMethod.Get, method)
    Assert.Equal("/posts", path)

// =============================================================================
// allRoutes enumeration tests
// =============================================================================

[<Fact>]
let ``allRoutes enumerates all route cases`` () =
    let routes = RouteReflection.allRoutes<PostRoute> ()
    routes |> should haveLength 5

[<Fact>]
let ``allRoutes enumerates nested routes`` () =
    let routes = RouteReflection.allRoutes<TestRoute> ()
    // TestRoute has: Api (containing Posts + Users), Health, Home
    // Posts: 5 cases, Users: 2 cases = 7 from Api + 2 top-level = 9 total
    routes |> should haveLength 9

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
    | PostRoute.Detail id -> Assert.Equal(Guid.Empty, id)
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

    invalidRoutes |> should be Empty
