module Falco.UnionRoutes.Tests.MatcherTests

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

type MatcherTestRoute =
    | [<Route(RouteMethod.Get, Path = "")>] Api of ApiRoute
    | [<Route(RouteMethod.Get, Path = "health")>] Health
    | [<Route(RouteMethod.Get, Path = "")>] Home

type PostId = PostId of Guid

type ConventionRoute =
    | Root
    | Show of id: PostId
    | [<Route(RouteMethod.Post)>] Create

type ConventionParent = | [<Route(Path = "blog")>] Blog of ConventionRoute

type AnyMethodRoute =
    | [<Route(RouteMethod.Any, Path = "catch-all")>] CatchAll
    | [<Route(RouteMethod.Get, Path = "get-only")>] GetOnly

type MultiParamRoute =
    | [<Route(RouteMethod.Get, Path = "orgs/{orgId}/repos/{repoId}")>] RepoDetail of orgId: Guid * repoId: Guid

    | [<Route(RouteMethod.Get, Path = "users/{userId}/posts/{postId}")>] UserPost of userId: int * postId: int

type TypeVarietyRoute =
    | [<Route(RouteMethod.Get, Path = "items/{id}")>] Int64Item of id: int64
    | [<Route(RouteMethod.Get, Path = "flags/{enabled}")>] Flag of enabled: bool
    | [<Route(RouteMethod.Get, Path = "slugs/{slug}")>] SlugItem of slug: string

type PreconditionMatchRoute =
    | [<Route(RouteMethod.Get, Path = "things/{id}")>] Thing of id: Guid * PreCondition<string>

// =============================================================================
// createMatcher tests
// =============================================================================

let testGuid = Guid.Parse("550e8400-e29b-41d4-a716-446655440000")

let matcher = Route.createMatcher<MatcherTestRoute> ()

[<Fact>]
let ``matches exact path`` () =
    test <@ matcher.Match(HttpMethod.Get, "/health") = Ok Health @>

[<Fact>]
let ``matches root path`` () =
    test <@ matcher.Match(HttpMethod.Get, "/") = Ok Home @>

[<Fact>]
let ``matches nested route with actual Guid parameter`` () =
    let guid = testGuid
    let result = matcher.Match(HttpMethod.Get, $"/posts/%O{guid}")
    test <@ result = Ok(Api(Posts(Detail guid))) @>

[<Fact>]
let ``matches POST method`` () =
    let result = matcher.Match(HttpMethod.Post, "/posts")
    test <@ result = Ok(Api(Posts PostRoute.Create)) @>

[<Fact>]
let ``matches PUT method with actual parameter`` () =
    let guid = testGuid
    let result = matcher.Match(HttpMethod.Put, $"/posts/%O{guid}")
    test <@ result = Ok(Api(Posts(Update guid))) @>

[<Fact>]
let ``matches DELETE method with actual parameter`` () =
    let guid = testGuid
    let result = matcher.Match(HttpMethod.Delete, $"/posts/%O{guid}")
    test <@ result = Ok(Api(Posts(Delete guid))) @>

[<Fact>]
let ``returns NoMatchingRoute for unknown path`` () =
    test <@ matcher.Match(HttpMethod.Get, "/nonexistent") = Error Route.NoMatchingRoute @>

[<Fact>]
let ``returns NoMatchingRoute for wrong method`` () =
    test <@ matcher.Match(HttpMethod.Delete, "/health") = Error Route.NoMatchingRoute @>

[<Fact>]
let ``returns ParameterError for invalid Guid`` () =
    match matcher.Match(HttpMethod.Get, "/posts/not-a-guid") with
    | Error(Route.ParameterError(_, _, "not-a-guid", "Guid")) -> ()
    | other -> failwith $"Expected ParameterError, got %A{other}"

[<Fact>]
let ``strips query string before matching`` () =
    test <@ matcher.Match(HttpMethod.Get, "/health?foo=bar") = Ok Health @>

[<Fact>]
let ``matches nested users route with actual parameter`` () =
    let guid = testGuid
    let result = matcher.Match(HttpMethod.Get, $"/users/%O{guid}")
    test <@ result = Ok(Api(Users(Profile guid))) @>

[<Fact>]
let ``matches GET list route`` () =
    let result = matcher.Match(HttpMethod.Get, "/posts")
    test <@ result = Ok(Api(Posts PostRoute.List)) @>

// =============================================================================
// Convention-based route matching
// =============================================================================

let conventionMatcher = Route.createMatcher<ConventionParent> ()

[<Fact>]
let ``matches convention Root`` () =
    test <@ conventionMatcher.Match(HttpMethod.Get, "/blog") = Ok(Blog Root) @>

[<Fact>]
let ``matches convention Show with actual single-case wrapper value`` () =
    let guid = testGuid
    let result = conventionMatcher.Match(HttpMethod.Get, $"/blog/%O{guid}")
    test <@ result = Ok(Blog(Show(PostId guid))) @>

[<Fact>]
let ``matches convention POST Create`` () =
    test <@ conventionMatcher.Match(HttpMethod.Post, "/blog") = Ok(Blog Create) @>

// =============================================================================
// Edge cases
// =============================================================================

[<Fact>]
let ``trailing slash matches same as without`` () =
    test <@ matcher.Match(HttpMethod.Get, "/health/") = Ok Health @>

[<Fact>]
let ``path matching is case-insensitive`` () =
    test <@ matcher.Match(HttpMethod.Get, "/HEALTH") = Ok Health @>
    test <@ matcher.Match(HttpMethod.Get, "/Health") = Ok Health @>
    test <@ matcher.Match(HttpMethod.Get, "/POSTS") = Ok(Api(Posts PostRoute.List)) @>

[<Fact>]
let ``empty URL matches root`` () =
    test <@ matcher.Match(HttpMethod.Get, "") = Ok Home @>

// =============================================================================
// HttpMethod.Any
// =============================================================================

let anyMatcher = Route.createMatcher<AnyMethodRoute> ()

[<Fact>]
let ``Any method route matches GET`` () =
    test <@ anyMatcher.Match(HttpMethod.Get, "/catch-all") = Ok CatchAll @>

[<Fact>]
let ``Any method route matches POST`` () =
    test <@ anyMatcher.Match(HttpMethod.Post, "/catch-all") = Ok CatchAll @>

[<Fact>]
let ``Any method route matches DELETE`` () =
    test <@ anyMatcher.Match(HttpMethod.Delete, "/catch-all") = Ok CatchAll @>

[<Fact>]
let ``specific method still enforced for non-Any routes`` () =
    test <@ anyMatcher.Match(HttpMethod.Post, "/get-only") = Error Route.NoMatchingRoute @>

// =============================================================================
// Multiple parameters
// =============================================================================

let multiMatcher = Route.createMatcher<MultiParamRoute> ()

[<Fact>]
let ``matches route with multiple Guid parameters`` () =
    let org = Guid.Parse("11111111-1111-1111-1111-111111111111")
    let repo = Guid.Parse("22222222-2222-2222-2222-222222222222")
    let result = multiMatcher.Match(HttpMethod.Get, $"/orgs/%O{org}/repos/%O{repo}")
    test <@ result = Ok(RepoDetail(org, repo)) @>

[<Fact>]
let ``matches route with multiple int parameters`` () =
    let result = multiMatcher.Match(HttpMethod.Get, "/users/42/posts/99")
    test <@ result = Ok(UserPost(42, 99)) @>

[<Fact>]
let ``returns ParameterError for invalid second parameter`` () =
    let org = Guid.Parse("11111111-1111-1111-1111-111111111111")

    match multiMatcher.Match(HttpMethod.Get, $"/orgs/%O{org}/repos/not-a-guid") with
    | Error(Route.ParameterError(_, _, "not-a-guid", "Guid")) -> ()
    | other -> failwith $"Expected ParameterError for second param, got %A{other}"

// =============================================================================
// Type variety (int64, bool, string params)
// =============================================================================

let typeMatcher = Route.createMatcher<TypeVarietyRoute> ()

[<Fact>]
let ``matches int64 parameter`` () =
    test <@ typeMatcher.Match(HttpMethod.Get, "/items/9999999999") = Ok(Int64Item 9999999999L) @>

[<Fact>]
let ``returns ParameterError for invalid int64`` () =
    match typeMatcher.Match(HttpMethod.Get, "/items/not-a-number") with
    | Error(Route.ParameterError(_, _, "not-a-number", "int64")) -> ()
    | other -> failwith $"Expected ParameterError for int64, got %A{other}"

[<Fact>]
let ``matches bool parameter`` () =
    test <@ typeMatcher.Match(HttpMethod.Get, "/flags/true") = Ok(Flag true) @>
    test <@ typeMatcher.Match(HttpMethod.Get, "/flags/false") = Ok(Flag false) @>

[<Fact>]
let ``returns ParameterError for invalid bool`` () =
    match typeMatcher.Match(HttpMethod.Get, "/flags/yes") with
    | Error(Route.ParameterError(_, _, "yes", "bool")) -> ()
    | other -> failwith $"Expected ParameterError for bool, got %A{other}"

[<Fact>]
let ``matches string parameter`` () =
    test <@ typeMatcher.Match(HttpMethod.Get, "/slugs/hello-world") = Ok(SlugItem "hello-world") @>

[<Fact>]
let ``returns ParameterError for invalid int`` () =
    match multiMatcher.Match(HttpMethod.Get, "/users/abc/posts/1") with
    | Error(Route.ParameterError(_, _, "abc", "int")) -> ()
    | other -> failwith $"Expected ParameterError for int, got %A{other}"

// =============================================================================
// Routes with preconditions (non-route fields preserved as defaults)
// =============================================================================

let preconditionMatcher = Route.createMatcher<PreconditionMatchRoute> ()

[<Fact>]
let ``matches route with precondition field`` () =
    let guid = testGuid

    match preconditionMatcher.Match(HttpMethod.Get, $"/things/%O{guid}") with
    | Ok(Thing(id, _)) -> test <@ id = guid @>
    | other -> failwith $"Expected Thing, got %A{other}"

// =============================================================================
// matchUrl convenience function
// =============================================================================

[<Fact>]
let ``matchUrl works as shorthand`` () =
    test <@ Route.matchUrl<MatcherTestRoute> HttpMethod.Get "/health" = Ok Health @>

[<Fact>]
let ``matchUrl returns error for no match`` () =
    test <@ Route.matchUrl<MatcherTestRoute> HttpMethod.Get "/nope" = Error Route.NoMatchingRoute @>

[<Fact>]
let ``matchUrl returns actual parameter values`` () =
    let guid = testGuid
    test <@ Route.matchUrl<MatcherTestRoute> HttpMethod.Get $"/posts/%O{guid}" = Ok(Api(Posts(Detail guid))) @>
