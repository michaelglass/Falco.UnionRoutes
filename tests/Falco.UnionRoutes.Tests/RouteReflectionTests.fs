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

[<Fact>]
let ``toKebabCase handles single letter words`` () =
    test <@ RouteReflection.toKebabCase "A" = "a" @>
    test <@ RouteReflection.toKebabCase "ATest" = "a-test" @>

[<Fact>]
let ``toKebabCase handles trailing acronyms`` () =
    test <@ RouteReflection.toKebabCase "GetAPI" = "get-api" @>
    test <@ RouteReflection.toKebabCase "UserID" = "user-id" @>

[<Fact>]
let ``toKebabCase handles strings with numbers`` () =
    // Numbers don't trigger splits on their own
    test <@ RouteReflection.toKebabCase "HTML5Parser" = "html5parser" @>
    test <@ RouteReflection.toKebabCase "V2Api" = "v2api" @>
    test <@ RouteReflection.toKebabCase "Get2FACode" = "get2fa-code" @>

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
            | PostRoute.List
            | PostRoute.Create
            | PostRoute.Update _
            | PostRoute.Delete _ -> false)

    match detailRoute with
    | PostRoute.Detail id -> test <@ id = Guid.Empty @>
    | PostRoute.List -> failwith "Expected Detail route, got List"
    | PostRoute.Create -> failwith "Expected Detail route, got Create"
    | PostRoute.Update id -> failwith $"Expected Detail route, got Update {id}"
    | PostRoute.Delete id -> failwith $"Expected Detail route, got Delete {id}"

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

// =============================================================================
// link function tests
// =============================================================================

[<Fact>]
let ``link generates concrete URL for parameterized route`` () =
    let id = Guid.Parse("12345678-1234-1234-1234-123456789abc")
    let url = RouteReflection.link (PostRoute.Detail id)
    test <@ url = "/posts/12345678-1234-1234-1234-123456789abc" @>

[<Fact>]
let ``link generates root path for List route`` () =
    let url = RouteReflection.link PostRoute.List
    test <@ url = "/posts" @>

[<Fact>]
let ``link generates correct path for nested routes`` () =
    let id = Guid.Parse("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
    let url = RouteReflection.link (TestRoute.Api(ApiRoute.Posts(PostRoute.Detail id)))
    test <@ url = "/posts/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" @>

[<Fact>]
let ``link returns root for empty path route`` () =
    let url = RouteReflection.link TestRoute.Home
    test <@ url = "/" @>

// =============================================================================
// Convention-based routing tests (no attributes)
// =============================================================================

/// Routes using naming conventions instead of attributes
type ConventionRoute =
    | Root
    | List
    | Create
    | Delete of id: Guid
    | Patch of id: Guid
    | DigestView
    | HealthCheck

[<Fact>]
let ``Root convention returns empty path with GET`` () =
    let info = RouteReflection.routeInfo ConventionRoute.Root
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/" @>

[<Fact>]
let ``List convention returns empty path with GET`` () =
    let info = RouteReflection.routeInfo ConventionRoute.List
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/" @>

[<Fact>]
let ``Create convention returns empty path with POST`` () =
    let info = RouteReflection.routeInfo ConventionRoute.Create
    test <@ info.Method = HttpMethod.Post @>
    test <@ info.Path = "/" @>

[<Fact>]
let ``Delete convention returns DELETE method`` () =
    let info = RouteReflection.routeInfo (ConventionRoute.Delete(Guid.Empty))
    test <@ info.Method = HttpMethod.Delete @>
    test <@ info.Path = "/{id}" @>

[<Fact>]
let ``Patch convention returns PATCH method`` () =
    let info = RouteReflection.routeInfo (ConventionRoute.Patch(Guid.Empty))
    test <@ info.Method = HttpMethod.Patch @>
    test <@ info.Path = "/{id}" @>

[<Fact>]
let ``Regular case name converts to kebab-case path`` () =
    let info = RouteReflection.routeInfo ConventionRoute.DigestView
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/digest-view" @>

[<Fact>]
let ``HealthCheck converts to health-check`` () =
    let info = RouteReflection.routeInfo ConventionRoute.HealthCheck
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/health-check" @>

// =============================================================================
// toHttpMethod tests
// =============================================================================

[<Fact>]
let ``toHttpMethod converts all RouteMethod values`` () =
    test <@ RouteReflection.toHttpMethod RouteMethod.Get = HttpMethod.Get @>
    test <@ RouteReflection.toHttpMethod RouteMethod.Post = HttpMethod.Post @>
    test <@ RouteReflection.toHttpMethod RouteMethod.Put = HttpMethod.Put @>
    test <@ RouteReflection.toHttpMethod RouteMethod.Delete = HttpMethod.Delete @>
    test <@ RouteReflection.toHttpMethod RouteMethod.Patch = HttpMethod.Patch @>
    test <@ RouteReflection.toHttpMethod RouteMethod.Any = HttpMethod.Any @>

// =============================================================================
// toFalcoMethod tests
// =============================================================================

[<Fact>]
let ``toFalcoMethod returns function for all HTTP methods`` () =
    // We can't compare functions directly, but we can verify they produce valid endpoints
    let testPath = "/test"

    let handler: Falco.HttpHandler =
        fun _ctx -> System.Threading.Tasks.Task.CompletedTask

    // Just verify each method returns a function that produces an endpoint without error
    let _getEndpoint = RouteReflection.toFalcoMethod HttpMethod.Get testPath handler
    let _postEndpoint = RouteReflection.toFalcoMethod HttpMethod.Post testPath handler
    let _putEndpoint = RouteReflection.toFalcoMethod HttpMethod.Put testPath handler

    let _deleteEndpoint =
        RouteReflection.toFalcoMethod HttpMethod.Delete testPath handler

    let _patchEndpoint = RouteReflection.toFalcoMethod HttpMethod.Patch testPath handler
    let _anyEndpoint = RouteReflection.toFalcoMethod HttpMethod.Any testPath handler
    // If we got here without exception, all methods work
    test <@ true @>

// =============================================================================
// endpoints function tests
// =============================================================================

type SimpleRoute =
    | Home
    | About

[<Fact>]
let ``endpoints generates HttpEndpoint list from handler`` () =
    let handler (_route: SimpleRoute) : Falco.HttpHandler =
        fun _ctx -> System.Threading.Tasks.Task.CompletedTask

    let endpoints = RouteReflection.endpoints handler
    test <@ List.length endpoints = 2 @>

// =============================================================================
// Multiple route parameters tests
// =============================================================================

type MultiParamRoute =
    | [<Route(RouteMethod.Get, Path = "{a}/{b}")>] TwoParams of a: Guid * b: Guid
    | Edit of a: Guid * b: int

[<Fact>]
let ``Route with multiple parameters includes all in path`` () =
    let info =
        RouteReflection.routeInfo (MultiParamRoute.TwoParams(Guid.Empty, Guid.Empty))

    test <@ info.Path = "/{a}/{b}" @>

[<Fact>]
let ``Convention route infers multiple parameters`` () =
    let info = RouteReflection.routeInfo (MultiParamRoute.Edit(Guid.Empty, 0))
    test <@ info.Path = "/{a}/{b}" @>

[<Fact>]
let ``link substitutes multiple parameters`` () =
    let a = Guid.Parse("11111111-1111-1111-1111-111111111111")
    let b = Guid.Parse("22222222-2222-2222-2222-222222222222")
    let url = RouteReflection.link (MultiParamRoute.TwoParams(a, b))
    test <@ url = "/11111111-1111-1111-1111-111111111111/22222222-2222-2222-2222-222222222222" @>

// =============================================================================
// Default value tests for allRoutes
// =============================================================================

type RouteWithStringParam = StringRoute of name: string

type RouteWithIntParam = IntRoute of count: int

type RouteWithInt64Param = Int64Route of bigNum: int64

[<Fact>]
let ``allRoutes uses empty string default for string params`` () =
    let routes = RouteReflection.allRoutes<RouteWithStringParam> ()

    match routes with
    | [ StringRoute name ] -> test <@ name = "" @>
    | _ -> failwith "Expected single route"

[<Fact>]
let ``allRoutes uses zero default for int params`` () =
    let routes = RouteReflection.allRoutes<RouteWithIntParam> ()

    match routes with
    | [ IntRoute count ] -> test <@ count = 0 @>
    | _ -> failwith "Expected single route"

[<Fact>]
let ``allRoutes uses zero default for int64 params`` () =
    let routes = RouteReflection.allRoutes<RouteWithInt64Param> ()

    match routes with
    | [ Int64Route bigNum ] -> test <@ bigNum = 0L @>
    | _ -> failwith "Expected single route"

// =============================================================================
// Regression tests
// =============================================================================

// Issue #1: Single-case DU wrapper types incorrectly treated as nested route unions
// https://github.com/michaelglass/Falco.UnionRoutes/issues/1

type WrappedPostId = WrappedPostId of Guid
type WrappedUserId = WrappedUserId of Guid

type Issue1Route =
    | [<Route(RouteMethod.Get, Path = "posts")>] List
    | [<Route(RouteMethod.Get, Path = "posts/{id}")>] Detail of Pre<WrappedUserId> * id: WrappedPostId

[<Fact>]
let ``Issue #1 - Single-case wrapper with Pre should not append {Item} to path`` () =
    let route =
        Issue1Route.Detail(Pre(WrappedUserId(Guid.NewGuid())), WrappedPostId(Guid.NewGuid()))

    let info = RouteReflection.routeInfo route
    // Should be /posts/{id}, not /posts/{id}/{Item}
    test <@ info.Path = "/posts/{id}" @>

[<Fact>]
let ``Issue #1 - link with single-case wrapper should not include Item`` () =
    let userId = Guid.Parse("11111111-1111-1111-1111-111111111111")
    let postId = Guid.Parse("22222222-2222-2222-2222-222222222222")
    let route = Issue1Route.Detail(Pre(WrappedUserId(userId)), WrappedPostId(postId))
    let url = RouteReflection.link route
    // Should substitute the postId, not include {Item} or extra segments
    test <@ url = "/posts/22222222-2222-2222-2222-222222222222" @>
