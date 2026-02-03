module Falco.UnionRoutes.Tests.RouteTests

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
    test <@ Route.toKebabCase "DigestView" = "digest-view" @>
    test <@ Route.toKebabCase "LoginPage" = "login-page" @>
    test <@ Route.toKebabCase "Dashboard" = "dashboard" @>

[<Fact>]
let ``toKebabCase handles single word`` () =
    test <@ Route.toKebabCase "Home" = "home" @>
    test <@ Route.toKebabCase "Profile" = "profile" @>

[<Fact>]
let ``toKebabCase handles consecutive capitals correctly`` () =
    // Consecutive capitals get split before final lowercase transition
    test <@ Route.toKebabCase "HTMLParser" = "html-parser" @>
    test <@ Route.toKebabCase "URLHandler" = "url-handler" @>
    test <@ Route.toKebabCase "GetAPIData" = "get-api-data" @>

[<Fact>]
let ``toKebabCase handles single letter words`` () =
    test <@ Route.toKebabCase "A" = "a" @>
    test <@ Route.toKebabCase "ATest" = "a-test" @>

[<Fact>]
let ``toKebabCase handles trailing acronyms`` () =
    test <@ Route.toKebabCase "GetAPI" = "get-api" @>
    test <@ Route.toKebabCase "UserID" = "user-id" @>

[<Fact>]
let ``toKebabCase handles strings with numbers`` () =
    // Numbers don't trigger splits on their own
    test <@ Route.toKebabCase "HTML5Parser" = "html5parser" @>
    test <@ Route.toKebabCase "V2Api" = "v2api" @>
    test <@ Route.toKebabCase "Get2FACode" = "get2fa-code" @>

// =============================================================================
// Simple route tests
// =============================================================================

[<Fact>]
let ``Health route returns correct path`` () =
    let info = Route.info TestRoute.Health
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/health" @>

[<Fact>]
let ``Home route with empty path returns root`` () =
    let info = Route.info TestRoute.Home
    test <@ info.Path = "/" @>

// =============================================================================
// Parameterized route tests
// =============================================================================

[<Fact>]
let ``PostRoute.List returns correct path`` () =
    let info = Route.info PostRoute.List
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/posts" @>

[<Fact>]
let ``PostRoute.Detail includes parameter placeholder`` () =
    let id = Guid.NewGuid()
    let info = Route.info (PostRoute.Detail id)
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/posts/{id}" @>

[<Fact>]
let ``PostRoute.Create uses POST method`` () =
    let info = Route.info PostRoute.Create
    test <@ info.Method = HttpMethod.Post @>

[<Fact>]
let ``PostRoute.Update uses PUT method`` () =
    let info = Route.info (PostRoute.Update(Guid.NewGuid()))
    test <@ info.Method = HttpMethod.Put @>

[<Fact>]
let ``PostRoute.Delete uses DELETE method`` () =
    let info = Route.info (PostRoute.Delete(Guid.NewGuid()))
    test <@ info.Method = HttpMethod.Delete @>

// =============================================================================
// Nested route tests
// =============================================================================

[<Fact>]
let ``Nested route combines paths correctly`` () =
    let info = Route.info (TestRoute.Api(ApiRoute.Posts PostRoute.List))
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/posts" @>

[<Fact>]
let ``Deeply nested route with param`` () =
    let id = Guid.NewGuid()

    let info = Route.info (TestRoute.Api(ApiRoute.Posts(PostRoute.Detail id)))

    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/posts/{id}" @>

[<Fact>]
let ``Nested route inherits method from leaf`` () =
    let info = Route.info (TestRoute.Api(ApiRoute.Posts PostRoute.Create))

    test <@ info.Method = HttpMethod.Post @>

// =============================================================================
// routeInfo (throwing version) tests
// =============================================================================

[<Fact>]
let ``routeInfo returns info for valid route`` () =
    let info = Route.info PostRoute.List
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/posts" @>

[<Fact>]
let ``routeTuple returns method and path`` () =
    let (method, path) = Route.routeTuple PostRoute.List
    test <@ method = HttpMethod.Get @>
    test <@ path = "/posts" @>

// =============================================================================
// allRoutes enumeration tests
// =============================================================================

[<Fact>]
let ``allRoutes enumerates all route cases`` () =
    let routes = Route.allRoutes<PostRoute> ()
    test <@ List.length routes = 5 @>

[<Fact>]
let ``allRoutes enumerates nested routes`` () =
    let routes = Route.allRoutes<TestRoute> ()
    // TestRoute has: Api (containing Posts + Users), Health, Home
    // Posts: 5 cases, Users: 2 cases = 7 from Api + 2 top-level = 9 total
    test <@ List.length routes = 9 @>

[<Fact>]
let ``allRoutes uses default values for parameters`` () =
    let routes = Route.allRoutes<PostRoute> ()

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
    let routes = Route.allRoutes<TestRoute> ()

    let invalidRoutes =
        routes
        |> List.choose (fun route ->
            match Route.tryRouteInfo route with
            | None -> Some $"Route missing attribute: {route}"
            | Some _ -> None)

    test <@ List.isEmpty invalidRoutes @>

// =============================================================================
// link function tests
// =============================================================================

[<Fact>]
let ``link generates concrete URL for parameterized route`` () =
    let id = Guid.Parse("12345678-1234-1234-1234-123456789abc")
    let url = Route.link (PostRoute.Detail id)
    test <@ url = "/posts/12345678-1234-1234-1234-123456789abc" @>

[<Fact>]
let ``link generates root path for List route`` () =
    let url = Route.link PostRoute.List
    test <@ url = "/posts" @>

[<Fact>]
let ``link generates correct path for nested routes`` () =
    let id = Guid.Parse("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
    let url = Route.link (TestRoute.Api(ApiRoute.Posts(PostRoute.Detail id)))
    test <@ url = "/posts/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" @>

[<Fact>]
let ``link returns root for empty path route`` () =
    let url = Route.link TestRoute.Home
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
    let info = Route.info ConventionRoute.Root
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/" @>

[<Fact>]
let ``List convention returns empty path with GET`` () =
    let info = Route.info ConventionRoute.List
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/" @>

[<Fact>]
let ``Create convention returns empty path with POST`` () =
    let info = Route.info ConventionRoute.Create
    test <@ info.Method = HttpMethod.Post @>
    test <@ info.Path = "/" @>

[<Fact>]
let ``Delete convention returns DELETE method`` () =
    let info = Route.info (ConventionRoute.Delete(Guid.Empty))
    test <@ info.Method = HttpMethod.Delete @>
    test <@ info.Path = "/{id}" @>

[<Fact>]
let ``Patch convention returns PATCH method`` () =
    let info = Route.info (ConventionRoute.Patch(Guid.Empty))
    test <@ info.Method = HttpMethod.Patch @>
    test <@ info.Path = "/{id}" @>

[<Fact>]
let ``Regular case name converts to kebab-case path`` () =
    let info = Route.info ConventionRoute.DigestView
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/digest-view" @>

[<Fact>]
let ``HealthCheck converts to health-check`` () =
    let info = Route.info ConventionRoute.HealthCheck
    test <@ info.Method = HttpMethod.Get @>
    test <@ info.Path = "/health-check" @>

// =============================================================================
// toHttpMethod tests
// =============================================================================

[<Fact>]
let ``toHttpMethod converts all RouteMethod values`` () =
    test <@ Route.toHttpMethod RouteMethod.Get = HttpMethod.Get @>
    test <@ Route.toHttpMethod RouteMethod.Post = HttpMethod.Post @>
    test <@ Route.toHttpMethod RouteMethod.Put = HttpMethod.Put @>
    test <@ Route.toHttpMethod RouteMethod.Delete = HttpMethod.Delete @>
    test <@ Route.toHttpMethod RouteMethod.Patch = HttpMethod.Patch @>
    test <@ Route.toHttpMethod RouteMethod.Any = HttpMethod.Any @>

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
    let _getEndpoint = Route.toFalcoMethod HttpMethod.Get testPath handler
    let _postEndpoint = Route.toFalcoMethod HttpMethod.Post testPath handler
    let _putEndpoint = Route.toFalcoMethod HttpMethod.Put testPath handler

    let _deleteEndpoint = Route.toFalcoMethod HttpMethod.Delete testPath handler

    let _patchEndpoint = Route.toFalcoMethod HttpMethod.Patch testPath handler
    let _anyEndpoint = Route.toFalcoMethod HttpMethod.Any testPath handler
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

    let endpoints = Route.endpoints handler
    test <@ List.length endpoints = 2 @>

// =============================================================================
// Multiple route parameters tests
// =============================================================================

type MultiParamRoute =
    | [<Route(RouteMethod.Get, Path = "{a}/{b}")>] TwoParams of a: Guid * b: Guid
    | Edit of a: Guid * b: int

[<Fact>]
let ``Route with multiple parameters includes all in path`` () =
    let info = Route.info (MultiParamRoute.TwoParams(Guid.Empty, Guid.Empty))

    test <@ info.Path = "/{a}/{b}" @>

[<Fact>]
let ``Convention route infers multiple parameters`` () =
    let info = Route.info (MultiParamRoute.Edit(Guid.Empty, 0))
    test <@ info.Path = "/{a}/{b}" @>

[<Fact>]
let ``link substitutes multiple parameters`` () =
    let a = Guid.Parse("11111111-1111-1111-1111-111111111111")
    let b = Guid.Parse("22222222-2222-2222-2222-222222222222")
    let url = Route.link (MultiParamRoute.TwoParams(a, b))
    test <@ url = "/11111111-1111-1111-1111-111111111111/22222222-2222-2222-2222-222222222222" @>

// =============================================================================
// Default value tests for allRoutes
// =============================================================================

type RouteWithStringParam = StringRoute of name: string

type RouteWithIntParam = IntRoute of count: int

type RouteWithInt64Param = Int64Route of bigNum: int64

[<Fact>]
let ``allRoutes uses empty string default for string params`` () =
    let routes = Route.allRoutes<RouteWithStringParam> ()

    match routes with
    | [ StringRoute name ] -> test <@ name = "" @>
    | _ -> failwith "Expected single route"

[<Fact>]
let ``allRoutes uses zero default for int params`` () =
    let routes = Route.allRoutes<RouteWithIntParam> ()

    match routes with
    | [ IntRoute count ] -> test <@ count = 0 @>
    | _ -> failwith "Expected single route"

[<Fact>]
let ``allRoutes uses zero default for int64 params`` () =
    let routes = Route.allRoutes<RouteWithInt64Param> ()

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
    | [<Route(RouteMethod.Get, Path = "posts/{id}")>] Detail of PreCondition<WrappedUserId> * id: WrappedPostId

[<Fact>]
let ``Issue #1 - Single-case wrapper with PreCondition should not append {Item} to path`` () =
    let route =
        Issue1Route.Detail(PreCondition(WrappedUserId(Guid.NewGuid())), WrappedPostId(Guid.NewGuid()))

    let info = Route.info route
    // Should be /posts/{id}, not /posts/{id}/{Item}
    test <@ info.Path = "/posts/{id}" @>

[<Fact>]
let ``Issue #1 - link with single-case wrapper should not include Item`` () =
    let userId = Guid.Parse("11111111-1111-1111-1111-111111111111")
    let postId = Guid.Parse("22222222-2222-2222-2222-222222222222")

    let route =
        Issue1Route.Detail(PreCondition(WrappedUserId(userId)), WrappedPostId(postId))

    let url = Route.link route
    // Should substitute the postId, not include {Item} or extra segments
    test <@ url = "/posts/22222222-2222-2222-2222-222222222222" @>

// =============================================================================
// Nested routes with params tests
// =============================================================================

// Test types for nested routes with params
type NestedItemId = NestedItemId of Guid

type UserItemRoute =
    | List
    | Detail of itemId: NestedItemId

type UserProfileRoute =
    | View
    | Edit

type NestedUserRoute =
    | Items of UserItemRoute
    | Profile of UserProfileRoute

type NestedParentRoute = Users of userId: Guid * NestedUserRoute

[<Fact>]
let ``Nested route with params includes case name in path`` () =
    let route =
        NestedParentRoute.Users(Guid.Empty, NestedUserRoute.Items UserItemRoute.List)

    let info = Route.info route
    // Should be /users/{userId}/items, not just /{userId}/items
    test <@ info.Path = "/users/{userId}/items" @>

[<Fact>]
let ``Nested route with params and child detail includes all segments`` () =
    let route =
        NestedParentRoute.Users(Guid.Empty, NestedUserRoute.Items(UserItemRoute.Detail(NestedItemId Guid.Empty)))

    let info = Route.info route
    // Should be /users/{userId}/items/{itemId}
    test <@ info.Path = "/users/{userId}/items/{itemId}" @>

[<Fact>]
let ``Nested route with params link substitutes values`` () =
    let userId = Guid.Parse("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
    let itemId = Guid.Parse("bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb")

    let route =
        NestedParentRoute.Users(userId, NestedUserRoute.Items(UserItemRoute.Detail(NestedItemId itemId)))

    let url = Route.link route
    test <@ url = "/users/aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa/items/bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb" @>

[<Fact>]
let ``Nested route with params enumerates all routes`` () =
    let routes = Route.allRoutes<NestedParentRoute> ()
    // Users has 2 children (Items, Profile), Items has 2 (List, Detail), Profile has 2 (View, Edit)
    // Total: 4 routes
    test <@ List.length routes = 4 @>

[<Fact>]
let ``Nested route without params still works (no case name added)`` () =
    let route = NestedUserRoute.Items UserItemRoute.List
    let info = Route.info route
    // No params on Items, so just uses kebab-case of case name
    test <@ info.Path = "/items" @>

// Issue #1 follow-up: Single-case route unions with only PreCondition<'T> field were incorrectly
// identified as "single-case wrapper types" because isSingleCaseWrapper wasn't checking
// if the inner type was a primitive. This caused route enumeration to fail.
// Example: type ApiAdminOrgRoute = Search of PreCondition<AdminUserId>
// Was incorrectly treated as a wrapper type like "PostId of Guid"

type AdminUserId = AdminUserId of Guid

/// Single-case route with only PreCondition<'T> - should NOT be treated as a wrapper type
type SingleCasePreConditionRoute = | [<Route(RouteMethod.Get, Path = "search")>] Search of PreCondition<AdminUserId>

/// Multiple single-case routes with PreCondition<'T> - all should enumerate correctly
type MultiSingleCasePreConditionRoutes =
    | [<Route(RouteMethod.Get, Path = "orgs/search")>] OrgSearch of PreCondition<AdminUserId>
    | [<Route(RouteMethod.Get, Path = "users/search")>] UserSearch of PreCondition<AdminUserId>
    | [<Route(RouteMethod.Get, Path = "metrics")>] Metrics of PreCondition<AdminUserId>

[<Fact>]
let ``Issue #1 - Single-case route with PreCondition should enumerate correctly`` () =
    let routes = Route.allRoutes<SingleCasePreConditionRoute> ()
    // Should enumerate to exactly 1 route, not 0 or error
    test <@ List.length routes = 1 @>

[<Fact>]
let ``Issue #1 - Single-case route with PreCondition should have correct path`` () =
    let route =
        SingleCasePreConditionRoute.Search(PreCondition(AdminUserId(Guid.NewGuid())))

    let info = Route.info route
    // Should be /search, not / or something with {Item}
    test <@ info.Path = "/search" @>

[<Fact>]
let ``Issue #1 - Multiple single-case PreCondition routes should all enumerate`` () =
    let routes = Route.allRoutes<MultiSingleCasePreConditionRoutes> ()
    // Should enumerate all 3 routes
    test <@ List.length routes = 3 @>

[<Fact>]
let ``Issue #1 - Multiple single-case PreCondition routes should have distinct paths`` () =
    let routes = Route.allRoutes<MultiSingleCasePreConditionRoutes> ()

    let paths = routes |> List.map (Route.info >> (fun i -> i.Path))
    // All paths should be different (not all mapping to "/")
    test <@ List.distinct paths |> List.length = 3 @>
    test <@ paths |> List.contains "/orgs/search" @>
    test <@ paths |> List.contains "/users/search" @>
    test <@ paths |> List.contains "/metrics" @>

// =============================================================================
// Route validation tests
// =============================================================================

/// Valid route for validation tests
type ValidRoute =
    | Home
    | Detail of id: Guid
    | Search of query: QueryParam<string>

[<Fact>]
let ``validateStructure returns Ok for valid routes`` () =
    let result = Route.validateStructure<ValidRoute> ()
    test <@ result = Ok() @>

[<Fact>]
let ``validateStructure returns Ok for nested routes`` () =
    let result = Route.validateStructure<TestRoute> ()
    test <@ result = Ok() @>

/// Route with invalid characters in path
type InvalidCharsRoute = | [<Route(Path = "hello world")>] WithSpace

[<Fact>]
let ``validateStructure catches invalid path characters`` () =
    let result = Route.validateStructure<InvalidCharsRoute> ()

    match result with
    | Error errors -> test <@ errors |> List.exists (fun e -> e.Contains("Invalid characters")) @>
    | Ok() -> failwith "Expected validation error for invalid characters"

/// Route with unbalanced braces
type UnbalancedBracesRoute =
    | [<Route(Path = "users/{id")>] MissingClose
    | [<Route(Path = "users/id}")>] MissingOpen

[<Fact>]
let ``validateStructure catches unbalanced braces`` () =
    let result = Route.validateStructure<UnbalancedBracesRoute> ()

    match result with
    | Error errors -> test <@ errors |> List.exists (fun e -> e.Contains("Unbalanced braces")) @>
    | Ok() -> failwith "Expected validation error for unbalanced braces"

/// Route with duplicate path params
type DuplicateParamsRoute = | [<Route(Path = "{id}/sub/{id}")>] Duplicate of id: Guid

[<Fact>]
let ``validateStructure catches duplicate path params`` () =
    let result = Route.validateStructure<DuplicateParamsRoute> ()

    match result with
    | Error errors -> test <@ errors |> List.exists (fun e -> e.Contains("Duplicate path parameters")) @>
    | Ok() -> failwith "Expected validation error for duplicate params"

/// Route with path param not matching field name
type MismatchedParamRoute = | [<Route(Path = "{userId}")>] Profile of id: Guid

[<Fact>]
let ``validateStructure catches path param not matching field name`` () =
    let result = Route.validateStructure<MismatchedParamRoute> ()

    match result with
    | Error errors -> test <@ errors |> List.exists (fun e -> e.Contains("not found in fields")) @>
    | Ok() -> failwith "Expected validation error for mismatched param"

/// Route with multiple nested unions (unsupported)
type ChildRouteA = | A

type ChildRouteB = | B

type MultipleNestedRoute = Both of ChildRouteA * ChildRouteB

[<Fact>]
let ``validateStructure catches multiple nested route unions`` () =
    let result = Route.validateStructure<MultipleNestedRoute> ()

    match result with
    | Error errors -> test <@ errors |> List.exists (fun e -> e.Contains("nested route unions")) @>
    | Ok() -> failwith "Expected validation error for multiple nested unions"

// =============================================================================
// RESTful case names with nested routes tests
// =============================================================================

type NestedChildRoute =
    | List
    | Detail of id: Guid

type ShowNestedRoute = Show of NestedChildRoute

type DeleteNestedRoute = Delete of NestedChildRoute

type EditNestedRoute = Edit of NestedChildRoute

[<Fact>]
let ``Show of nested route passes through without prefix`` () =
    let route = ShowNestedRoute.Show NestedChildRoute.List
    let info = Route.info route
    test <@ info.Path = "/" @>

[<Fact>]
let ``Show of nested route with params passes through`` () =
    let route = ShowNestedRoute.Show(NestedChildRoute.Detail Guid.Empty)
    let info = Route.info route
    // Detail has params, so it gets /detail/{id}
    test <@ info.Path = "/{id}" @>

[<Fact>]
let ``Delete of nested route passes through without prefix`` () =
    let route = DeleteNestedRoute.Delete NestedChildRoute.List
    let info = Route.info route
    test <@ info.Path = "/" @>
    // Method comes from leaf (List → GET), not parent
    test <@ info.Method = HttpMethod.Get @>

[<Fact>]
let ``Edit of nested route passes through without prefix`` () =
    let route = EditNestedRoute.Edit NestedChildRoute.List
    let info = Route.info route
    test <@ info.Path = "/" @>
