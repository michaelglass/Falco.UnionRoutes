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
open Falco.Markup
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

// Convention-based routes (no attributes needed for common patterns)
// Fields define what each route needs:
// - UserId fields are populated via auth pipeline
// - Guid fields are extracted from route parameters
type PostRoute =
    | List // GET /posts - no params
    | Detail of id: Guid // GET /posts/{id} - just the id
    | Create of UserId // POST /posts - requires auth
    | Delete of UserId * id: Guid // DELETE /posts/{id} - auth + id
    | Patch of UserId * id: Guid // PATCH /posts/{id} - auth + id

// Explicit attributes for custom paths or overriding conventions
type UserRoute =
    | [<Route(Path = "{userId}")>] Profile of userId: Guid // Custom param name
    | [<Route(RouteMethod.Put, Path = "{id}")>] Update of id: Guid // PUT instead of convention
    | [<Route(Path = "me")>] Me // Custom static path

// Mix of conventions and explicit attributes
type ApiRoute =
    | [<Route(RouteMethod.Any, Path = "webhook")>] Webhook // Any HTTP method
    | [<Route(Path = "v2/status")>] Status // Custom nested path

type Route =
    | Root
    | Posts of PostRoute
    | Users of UserRoute
    | Api of ApiRoute
    | Health

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
// 4. HTML Helpers
// =============================================================================

let layout (title: string) (content: XmlNode list) =
    Elem.html
        []
        [ Elem.head
              []
              [ Elem.title [] [ Text.raw title ]
                Elem.style
                    []
                    [ Text.raw
                          """
                body { font-family: system-ui, sans-serif; max-width: 800px; margin: 2rem auto; padding: 0 1rem; }
                h1 { color: #333; }
                a { color: #0066cc; }
                ul { line-height: 1.8; }
                code { background: #f4f4f4; padding: 0.2rem 0.4rem; border-radius: 3px; }
                .error { color: #cc0000; }
                .success { color: #008800; }
                pre { background: #f4f4f4; padding: 1rem; overflow-x: auto; }
                """ ] ]
          Elem.body [] content ]

let htmlResponse (title: string) (content: XmlNode list) : HttpHandler = layout title content |> Response.ofHtml

// =============================================================================
// 5. Error Response Handler
// =============================================================================
// Convert errors to HTTP responses

let toErrorResponse (error: AppError) : HttpHandler =
    let errorPage (code: int) (message: string) =
        Response.withStatusCode code
        >> Response.ofHtml (
            layout
                $"Error {code}"
                [ Elem.h1 [ Attr.class' "error" ] [ Text.raw $"Error {code}" ]
                  Elem.p [] [ Text.raw message ]
                  Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]
        )

    match error with
    | NotAuthenticated -> errorPage 401 "Unauthorized - X-User-Id header required"
    | Forbidden -> errorPage 403 "Forbidden"
    | NotFound msg -> errorPage 404 msg
    | BadRequest msg -> errorPage 400 msg

// =============================================================================
// 6. Auth Pipeline
// =============================================================================

/// Extract user from auth header (simplified example)
let requireAuth: Pipeline<UserId, AppError> =
    fun ctx ->
        match ctx.Request.Headers.TryGetValue("X-User-Id") with
        | true, values ->
            match Guid.TryParse(values.ToString()) with
            | true, guid -> Ok(UserId guid)
            | false, _ -> Error NotAuthenticated
        | false, _ -> Error NotAuthenticated

/// Require a user ID from route parameter (for UserRoute)
let requireUserIdFromRoute: Pipeline<UserId, AppError> =
    requireRouteId "userId" UserId (BadRequest "Invalid user ID")

// =============================================================================
// 6b. Route Hydration
// =============================================================================
// RouteHydration.create automatically extracts fields based on their types:
// - UserId fields use the auth pipeline
// - Guid fields are extracted from route params by field name

/// Hydrate PostRoute - extracts auth and route params automatically
let hydratePost: PostRoute -> Pipeline<PostRoute, AppError> =
    RouteHydration.create<PostRoute, UserId, AppError> requireAuth BadRequest

// =============================================================================
// 7. Handlers
// =============================================================================

module Handlers =
    let sampleId = Guid.Parse("11111111-1111-1111-1111-111111111111")

    /// Replace {param} placeholders with sample GUID for browsable links
    let makeBrowsablePath (path: string) =
        path.Replace("{id}", sampleId.ToString())

    /// Render a single route as an HTML list item
    let renderRoute (route: Route) =
        let info = RouteReflection.routeInfo route
        let methodStr = info.Method.ToString().ToUpper()
        let browsablePath = makeBrowsablePath info.Path

        match info.Method with
        | HttpMethod.Get -> Elem.li [] [ Elem.a [ Attr.href browsablePath ] [ Text.raw $"{methodStr} {info.Path}" ] ]
        | _ -> Elem.li [] [ Elem.code [] [ Text.raw $"{methodStr} {info.Path}" ]; Text.raw " (use curl)" ]

    let home: HttpHandler =
        let allRoutes = RouteReflection.allRoutes<Route> ()

        let getRoutes =
            allRoutes
            |> List.filter (fun r -> (RouteReflection.routeInfo r).Method = HttpMethod.Get)

        let otherRoutes =
            allRoutes
            |> List.filter (fun r -> (RouteReflection.routeInfo r).Method <> HttpMethod.Get)

        htmlResponse
            "Falco.UnionRoutes Example"
            [ Elem.h1 [] [ Text.raw "Falco.UnionRoutes Example" ]
              Elem.p [] [ Text.raw "This app demonstrates type-safe routing with discriminated unions." ]
              Elem.p
                  []
                  [ Text.raw $"Routes discovered via RouteReflection.allRoutes<Route>(): {allRoutes.Length} total" ]

              Elem.h2 [] [ Text.raw "Browsable Routes (GET)" ]
              Elem.ul [] (getRoutes |> List.map renderRoute)

              Elem.h2 [] [ Text.raw "Other Routes (require curl)" ]
              Elem.ul [] (otherRoutes |> List.map renderRoute)

              Elem.h2 [] [ Text.raw "Try with curl" ]
              Elem.pre
                  []
                  [ Text.raw
                        $"""# Create post (requires X-User-Id header)
curl -X POST http://localhost:5000/posts -H "X-User-Id: {sampleId}"

# Delete post (requires X-User-Id header)
curl -X DELETE http://localhost:5000/posts/{sampleId} -H "X-User-Id: {sampleId}" """ ] ]

    let health: HttpHandler = Response.ofJson {| status = "ok" |}

    let listPosts: HttpHandler =
        htmlResponse
            "Posts"
            [ Elem.h1 [] [ Text.raw "Posts" ]
              Elem.ul
                  []
                  [ Elem.li [] [ Elem.a [ Attr.href $"/posts/{sampleId}" ] [ Text.raw "Post 1: Introduction to F#" ] ]
                    Elem.li
                        []
                        [ Elem.a [ Attr.href $"/posts/{Guid.NewGuid()}" ] [ Text.raw "Post 2: Falco Web Framework" ] ] ]
              Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]

    let getPost (postId: PostId) : HttpHandler =
        let (PostId id) = postId

        htmlResponse
            "Post Detail"
            [ Elem.h1 [] [ Text.raw "Post Detail" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "Post ID: " ]
                    Elem.code [] [ Text.raw (id.ToString()) ] ]
              Elem.p
                  []
                  [ Text.raw "This post was loaded using a Pipeline to extract and validate the ID from the route." ]
              Elem.a [ Attr.href "/posts" ] [ Text.raw "Back to posts" ] ]

    let createPost (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        Response.withStatusCode 201
        >> Response.ofHtml (
            layout
                "Post Created"
                [ Elem.h1 [ Attr.class' "success" ] [ Text.raw "Post Created!" ]
                  Elem.p
                      []
                      [ Elem.strong [] [ Text.raw "Author ID: " ]
                        Elem.code [] [ Text.raw (uid.ToString()) ] ]
                  Elem.p
                      []
                      [ Text.raw "This handler used a Pipeline to extract the user ID from the X-User-Id header." ]
                  Elem.a [ Attr.href "/posts" ] [ Text.raw "View posts" ] ]
        )

    let deletePost (userId: UserId, postId: PostId) : HttpHandler =
        let (UserId uid) = userId
        let (PostId pid) = postId

        htmlResponse
            "Post Deleted"
            [ Elem.h1 [ Attr.class' "success" ] [ Text.raw "Post Deleted!" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "Post ID: " ]
                    Elem.code [] [ Text.raw (pid.ToString()) ] ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "Deleted by: " ]
                    Elem.code [] [ Text.raw (uid.ToString()) ] ]
              Elem.p
                  []
                  [ Text.raw "This handler used "
                    Elem.code [] [ Text.raw "requireAuth <&> requirePostId" ]
                    Text.raw " to compose two pipelines." ]
              Elem.a [ Attr.href "/posts" ] [ Text.raw "View posts" ] ]

    let getProfile (userId: UserId) : HttpHandler =
        let (UserId id) = userId

        htmlResponse
            "User Profile"
            [ Elem.h1 [] [ Text.raw "User Profile" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "User ID: " ]
                    Elem.code [] [ Text.raw (id.ToString()) ] ]
              Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]

    let patchPost (userId: UserId, postId: PostId) : HttpHandler =
        let (UserId uid) = userId
        let (PostId pid) = postId

        htmlResponse
            "Post Updated"
            [ Elem.h1 [ Attr.class' "success" ] [ Text.raw "Post Updated!" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "Post ID: " ]
                    Elem.code [] [ Text.raw (pid.ToString()) ] ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "Updated by: " ]
                    Elem.code [] [ Text.raw (uid.ToString()) ] ]
              Elem.a [ Attr.href "/posts" ] [ Text.raw "View posts" ] ]

    let updateUser (userId: UserId) : HttpHandler =
        let (UserId id) = userId
        htmlResponse "User Updated" [ Elem.h1 [] [ Text.raw $"Updated user {id}" ] ]

    let currentUser: HttpHandler =
        htmlResponse "Current User" [ Elem.h1 [] [ Text.raw "Current User (from session)" ] ]

    let webhook: HttpHandler = Response.ofJson {| received = true; method = "any" |}

    let apiStatus: HttpHandler = Response.ofJson {| version = "2.0"; status = "ok" |}

// =============================================================================
// 8. Post Handler (using hydration)
// =============================================================================
// The route is automatically hydrated - all fields are populated.
// Handler just pattern matches and uses the values directly.

let handlePost (route: PostRoute) : HttpHandler =
    match route with
    | PostRoute.List -> Handlers.listPosts
    | PostRoute.Detail id -> Handlers.getPost (PostId id)
    | PostRoute.Create userId -> Handlers.createPost userId
    | PostRoute.Delete(userId, id) -> Handlers.deletePost (userId, PostId id)
    | PostRoute.Patch(userId, id) -> Handlers.patchPost (userId, PostId id)

/// Combined: hydrate then handle
let postHandler (route: PostRoute) : HttpHandler =
    Pipeline.run toErrorResponse (hydratePost route) handlePost

// =============================================================================
// 9. Top-level Route Handler
// =============================================================================

let routeHandler (route: Route) : HttpHandler =
    match route with
    | Route.Root -> Handlers.home
    | Route.Health -> Handlers.health

    // PostRoute uses hydration
    | Route.Posts postRoute -> postHandler postRoute

    // User routes (still using manual pipelines for comparison)
    | Route.Users(UserRoute.Profile _) -> Pipeline.run toErrorResponse requireUserIdFromRoute Handlers.getProfile
    | Route.Users(UserRoute.Update _) ->
        Pipeline.run toErrorResponse (requireRouteId "id" UserId (BadRequest "Invalid user ID")) Handlers.updateUser
    | Route.Users UserRoute.Me -> Handlers.currentUser

    // API routes
    | Route.Api ApiRoute.Webhook -> Handlers.webhook
    | Route.Api ApiRoute.Status -> Handlers.apiStatus

// =============================================================================
// 10. Convert to Falco Endpoints
// =============================================================================

let endpoints = RouteReflection.endpoints routeHandler

// =============================================================================
// 11. Application Entry Point
// =============================================================================

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    let app = builder.Build()
    app.UseRouting() |> ignore
    app.UseFalco(endpoints) |> ignore
    app.Run()
    0
