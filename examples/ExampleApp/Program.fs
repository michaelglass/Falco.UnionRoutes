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
type AdminId = AdminId of Guid
type PostId = PostId of Guid
type Slug = Slug of string

// =============================================================================
// 2. Route Definitions
// =============================================================================
// Define routes as discriminated unions with [<Route>] attributes.
// - Use Path = "" for wrapper cases that group nested routes
// - Use {param} syntax for route parameters

// Convention-based routes (no attributes needed for common patterns)
// Fields define what each route needs:
// - Pre<UserId> fields come from preconditions (auth, validation, etc.)
// - Wrapper types like PostId (single-case DU of Guid) are auto-detected
// - Query<'T> fields extract from query string
// - Query<'T> option returns None when query param is missing
type PostRoute =
    | List of page: Query<int> option // GET /posts?page=2 - optional query param
    | Detail of id: PostId // GET /posts/{id} - extracts Guid, wraps as PostId
    | Create of Pre<UserId> // POST /posts - requires auth precondition
    | Delete of Pre<UserId> * id: PostId // DELETE /posts/{id} - auth + PostId
    | Patch of Pre<UserId> * id: PostId // PATCH /posts/{id} - auth + PostId
    | [<Route(Path = "search")>] Search of query: Query<string> // GET /posts/search?query=... - required query param

// Explicit attributes for custom paths or overriding conventions
// Also uses wrapper types (UserId wraps Guid)
type UserRoute =
    | [<Route(Path = "{userId}")>] Profile of userId: UserId // Custom param name, wrapper type
    | [<Route(RouteMethod.Put, Path = "{id}")>] Update of id: UserId // PUT instead of convention
    | [<Route(Path = "me")>] Me // Custom static path

// Mix of conventions and explicit attributes
// Also demonstrates custom type extraction with Slug
type ApiRoute =
    | [<Route(RouteMethod.Any, Path = "webhook")>] Webhook // Any HTTP method
    | [<Route(Path = "v2/status")>] Status // Custom nested path
    | [<Route(Path = "articles/{slug}")>] Article of slug: Slug // Custom type with custom extractor

// Multiple preconditions example
// Routes can require multiple different precondition types
type AdminRoute =
    | Dashboard of Pre<AdminId> // Admin auth only
    | [<Route(Path = "users")>] UserList of Pre<AdminId> // Admin auth only
    | [<Route(Path = "users/{id}")>] UserDetail of Pre<AdminId> * id: UserId // Admin + route param
    | [<Route(RouteMethod.Delete, Path = "users/{id}")>] BanUser of Pre<AdminId> * Pre<UserId> * id: Guid // Admin auth + current user + target user id

type Route =
    | Root
    | Posts of PostRoute
    | Users of UserRoute
    | Api of ApiRoute
    | Admin of AdminRoute
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

/// Extract admin from auth header (simplified example)
/// In a real app, this might check roles/permissions
let requireAdmin: Pipeline<AdminId, AppError> =
    fun ctx ->
        match ctx.Request.Headers.TryGetValue("X-Admin-Id") with
        | true, values ->
            match Guid.TryParse(values.ToString()) with
            | true, guid -> Ok(AdminId guid)
            | false, _ -> Error Forbidden
        | false, _ -> Error Forbidden

// =============================================================================
// 6b. Route Hydration
// =============================================================================
// RouteHydration.create automatically extracts fields based on their types:
// - Pre<'T> fields use matching preconditions (auth errors preserve their type)
// - Guid, string, int, int64, bool fields are extracted from route params
// - Wrapper types (single-case DUs like PostId) are auto-detected
// - Query<'T> fields extract from query string
// - Custom extractors can handle domain-specific types like Slug
// - All errors are accumulated and combined via combineErrors function

/// Convert extraction error string to AppError
let makeError (msg: string) = BadRequest msg

/// Combine multiple errors - preserves structure for pattern matching
let combineErrors (errors: AppError list) =
    match errors with
    | [ single ] -> single // Single error preserved as-is
    | multiple -> BadRequest(multiple |> List.map string |> String.concat "; ")

/// Create auth precondition for Pre<UserId>
/// The result is automatically wrapped in Pre when extracted
let authPrecondition () = RouteHydration.forPre<UserId, AppError> requireAuth

/// Create admin precondition for Pre<AdminId>
let adminPrecondition () = RouteHydration.forPre<AdminId, AppError> requireAdmin

/// Hydrate PostRoute - extracts auth and route params automatically
let hydratePost: PostRoute -> Pipeline<PostRoute, AppError> =
    RouteHydration.create<PostRoute, AppError> [ authPrecondition () ] [] makeError combineErrors

/// Hydrate UserRoute - no preconditions needed, just extracts route params
let hydrateUser: UserRoute -> Pipeline<UserRoute, AppError> =
    RouteHydration.create<UserRoute, AppError> [] [] makeError combineErrors

/// Custom extractor for Slug type - demonstrates extensibility
let slugExtractor: TypeExtractor =
    fun fieldName fieldType ctx ->
        if fieldType = typeof<Slug> then
            let route = Request.getRoute ctx
            let value = route.GetString fieldName

            if String.IsNullOrEmpty(value) then
                Some(Error $"Missing slug: {fieldName}")
            else
                Some(Ok(box (Slug value)))
        else
            None // Can't handle this type, defer to built-in extractors

/// Hydrate ApiRoute - uses custom extractor for Slug type
let hydrateApi: ApiRoute -> Pipeline<ApiRoute, AppError> =
    RouteHydration.create<ApiRoute, AppError> [] [ slugExtractor ] makeError combineErrors

/// Hydrate AdminRoute - uses BOTH user and admin preconditions
/// Routes can require multiple different precondition types
let hydrateAdmin: AdminRoute -> Pipeline<AdminRoute, AppError> =
    RouteHydration.create<AdminRoute, AppError>
        [ adminPrecondition (); authPrecondition () ] // Both preconditions available
        []
        makeError
        combineErrors

// =============================================================================
// 7. Handlers
// =============================================================================

module Handlers =
    let sampleId = Guid.Parse("11111111-1111-1111-1111-111111111111")

    /// Replace {param} placeholders with sample GUID for browsable links
    let makeBrowsablePath (path: string) =
        path
            .Replace("{id}", sampleId.ToString())
            .Replace("{userId}", sampleId.ToString())
            .Replace("{slug}", "hello-world")

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
curl -X DELETE http://localhost:5000/posts/{sampleId} -H "X-User-Id: {sampleId}"

# Admin dashboard (requires X-Admin-Id header)
curl http://localhost:5000/admin -H "X-Admin-Id: {sampleId}"

# Ban user (requires BOTH X-Admin-Id AND X-User-Id headers)
# This demonstrates multiple preconditions: Pre<AdminId> * Pre<UserId> * id
curl -X DELETE http://localhost:5000/admin/users/{sampleId} \
  -H "X-Admin-Id: {sampleId}" \
  -H "X-User-Id: {sampleId}" """ ] ]

    let health: HttpHandler = Response.ofJson {| status = "ok" |}

    let listPosts (page: Query<int> option) : HttpHandler =
        let pageNum = page |> Option.map (fun (Query p) -> p) |> Option.defaultValue 1

        htmlResponse
            "Posts"
            [ Elem.h1 [] [ Text.raw "Posts" ]
              Elem.p [] [ Text.raw $"Page {pageNum}" ]
              Elem.p
                  []
                  [ Text.raw "Try: "
                    Elem.a [ Attr.href "/posts?page=2" ] [ Text.raw "/posts?page=2" ] ]
              Elem.ul
                  []
                  [ Elem.li [] [ Elem.a [ Attr.href $"/posts/{sampleId}" ] [ Text.raw "Post 1: Introduction to F#" ] ]
                    Elem.li
                        []
                        [ Elem.a [ Attr.href $"/posts/{Guid.NewGuid()}" ] [ Text.raw "Post 2: Falco Web Framework" ] ] ]
              Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]

    let searchPosts (query: Query<string>) : HttpHandler =
        let (Query q) = query

        htmlResponse
            "Search Results"
            [ Elem.h1 [] [ Text.raw "Search Results" ]
              Elem.p [] [ Elem.strong [] [ Text.raw "Query: " ]; Elem.code [] [ Text.raw q ] ]
              Elem.p [] [ Text.raw "This demonstrates Query<'T> extraction from query string." ]
              Elem.a [ Attr.href "/posts" ] [ Text.raw "Back to posts" ] ]

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

    let article (slug: Slug) : HttpHandler =
        let (Slug s) = slug

        htmlResponse
            "Article"
            [ Elem.h1 [] [ Text.raw "Article" ]
              Elem.p [] [ Elem.strong [] [ Text.raw "Slug: " ]; Elem.code [] [ Text.raw s ] ]
              Elem.p [] [ Text.raw "This demonstrates custom type extraction with a Slug type." ]
              Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]

    // Admin handlers - demonstrate multiple preconditions
    let adminDashboard (adminId: AdminId) : HttpHandler =
        let (AdminId aid) = adminId

        htmlResponse
            "Admin Dashboard"
            [ Elem.h1 [] [ Text.raw "Admin Dashboard" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "Admin ID: " ]
                    Elem.code [] [ Text.raw (aid.ToString()) ] ]
              Elem.p [] [ Text.raw "This route requires Pre<AdminId> - admin authentication." ]
              Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]

    let adminUserList (adminId: AdminId) : HttpHandler =
        let (AdminId aid) = adminId

        htmlResponse
            "User Management"
            [ Elem.h1 [] [ Text.raw "User Management" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "Admin: " ]
                    Elem.code [] [ Text.raw (aid.ToString()) ] ]
              Elem.ul
                  []
                  [ Elem.li [] [ Text.raw "User 1: alice@example.com" ]
                    Elem.li [] [ Text.raw "User 2: bob@example.com" ] ]
              Elem.a [ Attr.href "/admin" ] [ Text.raw "Back to admin" ] ]

    let adminUserDetail (adminId: AdminId, userId: UserId) : HttpHandler =
        let (AdminId aid) = adminId
        let (UserId uid) = userId

        htmlResponse
            "User Detail"
            [ Elem.h1 [] [ Text.raw "User Detail (Admin View)" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "Viewing User: " ]
                    Elem.code [] [ Text.raw (uid.ToString()) ] ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "Admin: " ]
                    Elem.code [] [ Text.raw (aid.ToString()) ] ]
              Elem.p [] [ Text.raw "This route uses Pre<AdminId> + id: UserId (route param wrapped as UserId)." ]
              Elem.a [ Attr.href "/admin/users" ] [ Text.raw "Back to user list" ] ]

    let banUser (adminId: AdminId, actingUserId: UserId, targetUserId: Guid) : HttpHandler =
        let (AdminId aid) = adminId
        let (UserId uid) = actingUserId

        htmlResponse
            "User Banned"
            [ Elem.h1 [ Attr.class' "error" ] [ Text.raw "User Banned!" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "Banned User: " ]
                    Elem.code [] [ Text.raw (targetUserId.ToString()) ] ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "Admin: " ]
                    Elem.code [] [ Text.raw (aid.ToString()) ] ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "Acting User: " ]
                    Elem.code [] [ Text.raw (uid.ToString()) ] ]
              Elem.p
                  []
                  [ Text.raw "This route demonstrates "
                    Elem.strong [] [ Text.raw "multiple preconditions" ]
                    Text.raw ": Pre<AdminId> * Pre<UserId> * id: Guid" ]
              Elem.a [ Attr.href "/admin/users" ] [ Text.raw "Back to user list" ] ]

// =============================================================================
// 8. Post Handler (using hydration)
// =============================================================================
// The route is automatically hydrated - all fields are populated.
// Handler just pattern matches and uses the values directly.

let handlePost (route: PostRoute) : HttpHandler =
    match route with
    | PostRoute.List page -> Handlers.listPosts page
    | PostRoute.Detail postId -> Handlers.getPost postId
    | PostRoute.Create(Pre userId) -> Handlers.createPost userId
    | PostRoute.Delete(Pre userId, postId) -> Handlers.deletePost (userId, postId)
    | PostRoute.Patch(Pre userId, postId) -> Handlers.patchPost (userId, postId)
    | PostRoute.Search query -> Handlers.searchPosts query

/// Combined: hydrate then handle
let postHandler (route: PostRoute) : HttpHandler =
    Pipeline.run toErrorResponse (hydratePost route) handlePost

// =============================================================================
// 8b. User Handler (using hydration)
// =============================================================================

let handleUser (route: UserRoute) : HttpHandler =
    match route with
    | UserRoute.Profile userId -> Handlers.getProfile userId
    | UserRoute.Update userId -> Handlers.updateUser userId
    | UserRoute.Me -> Handlers.currentUser

/// Combined: hydrate then handle
let userHandler (route: UserRoute) : HttpHandler =
    Pipeline.run toErrorResponse (hydrateUser route) handleUser

// =============================================================================
// 8c. Api Handler (using hydration with custom extractor)
// =============================================================================

let handleApi (route: ApiRoute) : HttpHandler =
    match route with
    | ApiRoute.Webhook -> Handlers.webhook
    | ApiRoute.Status -> Handlers.apiStatus
    | ApiRoute.Article slug -> Handlers.article slug

/// Combined: hydrate then handle (uses custom Slug extractor)
let apiHandler (route: ApiRoute) : HttpHandler =
    Pipeline.run toErrorResponse (hydrateApi route) handleApi

// =============================================================================
// 8d. Admin Handler (using hydration with MULTIPLE preconditions)
// =============================================================================
// Demonstrates routes that require different combinations of preconditions

let handleAdmin (route: AdminRoute) : HttpHandler =
    match route with
    | AdminRoute.Dashboard(Pre adminId) -> Handlers.adminDashboard adminId
    | AdminRoute.UserList(Pre adminId) -> Handlers.adminUserList adminId
    | AdminRoute.UserDetail(Pre adminId, userId) -> Handlers.adminUserDetail (adminId, userId)
    | AdminRoute.BanUser(Pre adminId, Pre userId, targetId) -> Handlers.banUser (adminId, userId, targetId)

/// Combined: hydrate then handle (uses both admin and user preconditions)
let adminHandler (route: AdminRoute) : HttpHandler =
    Pipeline.run toErrorResponse (hydrateAdmin route) handleAdmin

// =============================================================================
// 9. Top-level Route Handler
// =============================================================================

let routeHandler (route: Route) : HttpHandler =
    match route with
    | Route.Root -> Handlers.home
    | Route.Health -> Handlers.health

    // PostRoute uses hydration (with auth)
    | Route.Posts postRoute -> postHandler postRoute

    // UserRoute uses hydration (no auth, just route params)
    | Route.Users userRoute -> userHandler userRoute

    // ApiRoute uses hydration with custom Slug extractor
    | Route.Api apiRoute -> apiHandler apiRoute

    // AdminRoute uses hydration with MULTIPLE preconditions (admin + user)
    | Route.Admin adminRoute -> adminHandler adminRoute

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
