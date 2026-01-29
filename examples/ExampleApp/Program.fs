// =============================================================================
// Falco.UnionRoutes Example App
// =============================================================================
// This example demonstrates all features of the Falco.UnionRoutes library:
// - Type-safe route definitions with discriminated unions
// - Nested route hierarchies
// - Railway-oriented pipeline composition
// - Route parameter extraction
// - Error handling

open System
open Falco
open Falco.Routing
open Falco.UnionRoutes
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http

// =============================================================================
// 1. Domain Types
// =============================================================================
// Use single-case unions for type-safe IDs

type UserId = UserId of Guid
type PostId = PostId of Guid

// =============================================================================
// 2. Route Definitions
// =============================================================================
// Define routes as discriminated unions with [<Route>] attributes.
// - Use Path = "" for wrapper cases that group nested routes
// - Use {param} syntax for route parameters

type PostRoute =
    | [<Route(RouteMethod.Get, Path = "posts")>] List
    | [<Route(RouteMethod.Get, Path = "posts/{id}")>] Detail of id: Guid
    | [<Route(RouteMethod.Post, Path = "posts")>] Create
    | [<Route(RouteMethod.Delete, Path = "posts/{id}")>] Delete of id: Guid

type UserRoute = | [<Route(RouteMethod.Get, Path = "users/{id}")>] Profile of id: Guid

type Route =
    | [<Route(RouteMethod.Get, Path = "")>] Posts of PostRoute
    | [<Route(RouteMethod.Get, Path = "")>] Users of UserRoute
    | [<Route(RouteMethod.Get, Path = "health")>] Health

// =============================================================================
// 3. Error Type
// =============================================================================
// Define an error type for your application

type AppError =
    | NotAuthenticated
    | Forbidden
    | NotFound of string
    | BadRequest of string

// =============================================================================
// 4. Error Response Handler
// =============================================================================
// Convert errors to HTTP responses

let toErrorResponse (error: AppError) : HttpHandler =
    match error with
    | NotAuthenticated -> Response.withStatusCode 401 >> Response.ofPlainText "Unauthorized"
    | Forbidden -> Response.withStatusCode 403 >> Response.ofPlainText "Forbidden"
    | NotFound msg -> Response.withStatusCode 404 >> Response.ofPlainText msg
    | BadRequest msg -> Response.withStatusCode 400 >> Response.ofPlainText msg

// =============================================================================
// 5. Pipelines
// =============================================================================
// Pipelines extract and validate data from HTTP requests.
// They short-circuit on the first error.

/// Extract user from auth header (simplified example)
let requireAuth: Pipeline<UserId, AppError> =
    fun ctx ->
        match ctx.Request.Headers.TryGetValue("X-User-Id") with
        | true, values ->
            match Guid.TryParse(values.ToString()) with
            | true, guid -> Ok(UserId guid)
            | false, _ -> Error NotAuthenticated
        | false, _ -> Error NotAuthenticated

/// Require a post ID from route parameter
let requirePostId: Pipeline<PostId, AppError> =
    requireRouteId "id" PostId (BadRequest "Invalid post ID")

/// Require a user ID from route parameter
let requireUserId: Pipeline<UserId, AppError> =
    requireRouteId "id" UserId (BadRequest "Invalid user ID")

// =============================================================================
// 6. Handlers
// =============================================================================

module Handlers =
    let health: HttpHandler = Response.ofJson {| status = "ok" |}

    let listPosts: HttpHandler = Response.ofJson {| posts = [ "Post 1"; "Post 2" ] |}

    let getPost (postId: PostId) : HttpHandler =
        let (PostId id) = postId
        Response.ofJson {| id = id; title = "Example Post" |}

    let createPost (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        Response.withStatusCode 201
        >> Response.ofJson {| message = "Created"; author = uid |}

    let deletePost (userId: UserId, postId: PostId) : HttpHandler =
        let (UserId uid) = userId
        let (PostId pid) = postId
        Response.ofJson {| deleted = pid; by = uid |}

    let getProfile (userId: UserId) : HttpHandler =
        let (UserId id) = userId
        Response.ofJson {| id = id; name = "User" |}

// =============================================================================
// 7. Route Handler
// =============================================================================
// Map routes to handlers, using pipelines for validation

let routeHandler (route: Route) : HttpHandler =
    match route with
    | Route.Health -> Handlers.health

    // Public routes - no authentication required
    | Route.Posts PostRoute.List -> Handlers.listPosts

    // Single pipeline - extract post ID
    | Route.Posts(PostRoute.Detail _) -> Pipeline.run toErrorResponse requirePostId Handlers.getPost

    // Single pipeline - require authentication
    | Route.Posts PostRoute.Create -> Pipeline.run toErrorResponse requireAuth Handlers.createPost

    // Composed pipelines - require both auth AND post ID
    | Route.Posts(PostRoute.Delete _) ->
        Pipeline.run toErrorResponse (requireAuth <&> requirePostId) Handlers.deletePost

    | Route.Users(UserRoute.Profile _) -> Pipeline.run toErrorResponse requireUserId Handlers.getProfile

// =============================================================================
// 8. Convert to Falco Endpoints
// =============================================================================

let toFalcoMethod (method: HttpMethod) =
    match method with
    | HttpMethod.Get -> get
    | HttpMethod.Post -> post
    | HttpMethod.Put -> put
    | HttpMethod.Delete -> delete
    | HttpMethod.Patch -> patch

let endpoints =
    RouteReflection.allRoutes<Route> ()
    |> List.map (fun route ->
        let info = RouteReflection.routeInfo route
        let handler = routeHandler route
        toFalcoMethod info.Method info.Path handler)

// =============================================================================
// 9. Application Entry Point
// =============================================================================

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    let app = builder.Build()
    app.UseRouting() |> ignore
    app.UseFalco(endpoints) |> ignore
    app.Run()
    0
