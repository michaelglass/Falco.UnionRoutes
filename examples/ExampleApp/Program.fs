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
// - PreCondition<UserId> fields come from preconditions (auth, validation, etc.)
// - Wrapper types like PostId (single-case DU of Guid) are auto-detected
// - QueryParam<'T> fields extract from query string
// - QueryParam<'T> option returns None when query param is missing
type PostRoute =
    | List of page: QueryParam<int> option // GET /posts?page=2 - optional query param
    | Detail of id: PostId // GET /posts/{id} - extracts Guid, wraps as PostId
    | Create of PreCondition<UserId> // POST /posts - requires auth precondition
    | Delete of PreCondition<UserId> * id: PostId // DELETE /posts/{id} - auth + PostId
    | Patch of PreCondition<UserId> * id: PostId // PATCH /posts/{id} - auth + PostId
    | Search of query: QueryParam<string> // GET /posts/search?query=... - required query param

// Explicit attributes for custom paths or overriding conventions
// Also uses wrapper types (UserId wraps Guid)
type UserRoute =
    | [<Route(Path = "{userId}")>] Profile of userId: UserId // Custom param name (not {id}), wrapper type
    | [<Route(RouteMethod.Put)>] Update of id: UserId // PUT instead of convention (path auto-generated as /{id})
    | Me // GET /users/me - convention-based

// Mix of conventions and explicit attributes
// Also demonstrates custom type extraction with Slug
type ApiRoute =
    | [<Route(RouteMethod.Any)>] Webhook // Any HTTP method, path is /api/webhook by convention
    | [<Route(Path = "v2/status")>] Status // Custom path different from route name
    | [<Route(Path = "articles/{slug}")>] Article of slug: Slug // Custom path with custom type extractor

// Multiple preconditions example
// Routes can require multiple different precondition types
type AdminRoute =
    | Dashboard of PreCondition<AdminId> // GET /admin/dashboard - admin auth only
    | Users of PreCondition<AdminId> // GET /admin/users - admin auth only
    | [<Route(Path = "users/{id}")>] UserDetail of PreCondition<AdminId> * id: UserId // Custom path for nested resource
    | [<Route(RouteMethod.Delete, Path = "users/{id}")>] BanUser of
        PreCondition<AdminId> *
        PreCondition<UserId> *
        id: Guid // DELETE method + custom path

// =============================================================================
// Nested routes with params example - /users/{userId}/items/{itemId}
// =============================================================================
// Demonstrates parent params + child routes pattern

type ItemId = ItemId of Guid

type UserItemRoute =
    | List // GET /nested-user/{userId}/items - inherits OptionalPreCondition<AdminId>
    | Show of itemId: ItemId // GET /nested-user/{userId}/items/{itemId} - "Show" convention (no /show prefix)
    | [<SkipAllPreconditions>] Public // skips ALL optional preconditions
    | [<SkipPrecondition(typeof<AdminId>)>] Limited // skips only OptionalPreCondition<AdminId>

type UserDashboardRoute =
    | Overview // GET /users/{userId}/dashboard
    | Settings // GET /users/{userId}/dashboard/settings

type NestedUserRoute =
    | Items of UserItemRoute
    | Dashboard of UserDashboardRoute

type UserWithParamsRoute = NestedUser of userId: UserId * OverridablePreCondition<AdminId> * NestedUserRoute // Parent param + skippable precondition

// Path-less group example - groups routes without adding a path segment
// The empty Path = "" on Route means these routes appear at root level (no /internal prefix)
// Each single-case route with only PreCondition<'T> is correctly handled (not treated as a wrapper type)
type InternalApiRoute =
    | Metrics of PreCondition<AdminId> // GET /metrics - convention-based path
    | [<Route(Path = "health-deep")>] DeepHealth of PreCondition<AdminId> // GET /health-deep - custom path (not /deep-health)
    | [<Route(Path = "cache/clear")>] ClearCache of PreCondition<AdminId> // GET /cache/clear - nested custom path

type Route =
    | Root
    | Posts of PostRoute
    | Users of UserRoute
    | Api of ApiRoute
    | Admin of AdminRoute
    | [<Route(Path = "")>] Internal of InternalApiRoute // Path-less group - no path segment added
    | Health
    | UserWithParams of UserWithParamsRoute // Nested routes with params demo

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
let requireAuth: Extractor<UserId, AppError> =
    fun ctx ->
        match ctx.Request.Headers.TryGetValue("X-User-Id") with
        | true, values ->
            match Guid.TryParse(values.ToString()) with
            | true, guid -> Ok(UserId guid)
            | false, _ -> Error NotAuthenticated
        | false, _ -> Error NotAuthenticated

/// Extract admin from auth header (simplified example)
/// In a real app, this might check roles/permissions
let requireAdmin: Extractor<AdminId, AppError> =
    fun ctx ->
        match ctx.Request.Headers.TryGetValue("X-Admin-Id") with
        | true, values ->
            match Guid.TryParse(values.ToString()) with
            | true, guid -> Ok(AdminId guid)
            | false, _ -> Error Forbidden
        | false, _ -> Error Forbidden

// =============================================================================
// 6b. Endpoint Configuration
// =============================================================================
// Route.endpoints takes a config record that bundles all extraction settings:
// - Preconditions for auth/validation (PreCondition<'T> and OverridablePreCondition<'T>)
// - Custom parsers for domain types (like Slug)
// - Error handling functions
//
// Hydration is RECURSIVE - nested route unions are automatically hydrated!

/// Custom parser for Slug type - demonstrates extensibility
let slugParser: Parser<Slug> = fun s -> Ok(Slug s)

/// Endpoint configuration - bundles all extraction settings in one place
let endpointConfig: EndpointConfig<AppError> =
    { Preconditions =
        [ Extractor.precondition<UserId, AppError> requireAuth
          Extractor.precondition<AdminId, AppError> requireAdmin
          Extractor.overridablePrecondition<AdminId, AppError> requireAdmin ]
      Parsers = [ Extractor.parser slugParser ]
      MakeError = fun msg -> BadRequest msg
      CombineErrors =
        fun errors ->
            match errors with
            | [ single ] -> single
            | multiple -> BadRequest(multiple |> List.map string |> String.concat "; ")
      ToErrorResponse = toErrorResponse }

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
        let info = Route.info route
        let methodStr = info.Method.ToString().ToUpper()
        let browsablePath = makeBrowsablePath info.Path

        match info.Method with
        | HttpMethod.Get -> Elem.li [] [ Elem.a [ Attr.href browsablePath ] [ Text.raw $"{methodStr} {info.Path}" ] ]
        | _ -> Elem.li [] [ Elem.code [] [ Text.raw $"{methodStr} {info.Path}" ]; Text.raw " (use curl)" ]

    let home: HttpHandler =
        let allRoutes = Route.allRoutes<Route> ()

        let getRoutes =
            allRoutes |> List.filter (fun r -> (Route.info r).Method = HttpMethod.Get)

        let otherRoutes =
            allRoutes |> List.filter (fun r -> (Route.info r).Method <> HttpMethod.Get)

        htmlResponse
            "Falco.UnionRoutes Example"
            [ Elem.h1 [] [ Text.raw "Falco.UnionRoutes Example" ]
              Elem.p [] [ Text.raw "This app demonstrates type-safe routing with discriminated unions." ]
              Elem.p [] [ Text.raw $"Routes discovered via Route.allRoutes<Route>(): {allRoutes.Length} total" ]

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
# This demonstrates multiple preconditions: PreCondition<AdminId> * PreCondition<UserId> * id
curl -X DELETE http://localhost:5000/admin/users/{sampleId} \
  -H "X-Admin-Id: {sampleId}" \
  -H "X-User-Id: {sampleId}" """ ] ]

    let health: HttpHandler = Response.ofJson {| status = "ok" |}

    let listPosts (page: QueryParam<int> option) : HttpHandler =
        let pageNum = page |> Option.map (fun (QueryParam p) -> p) |> Option.defaultValue 1

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

    let searchPosts (query: QueryParam<string>) : HttpHandler =
        let (QueryParam q) = query

        htmlResponse
            "Search Results"
            [ Elem.h1 [] [ Text.raw "Search Results" ]
              Elem.p [] [ Elem.strong [] [ Text.raw "Query: " ]; Elem.code [] [ Text.raw q ] ]
              Elem.p [] [ Text.raw "This demonstrates QueryParam<'T> extraction from query string." ]
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
                  [ Text.raw "Auth and route params are extracted automatically via "
                    Elem.code [] [ Text.raw "Route.endpoints" ]
                    Text.raw "." ]
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
              Elem.p [] [ Text.raw "This route requires PreCondition<AdminId> - admin authentication." ]
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
              Elem.p
                  []
                  [ Text.raw "This route uses PreCondition<AdminId> + id: UserId (route param wrapped as UserId)." ]
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
                    Text.raw ": PreCondition<AdminId> * PreCondition<UserId> * id: Guid" ]
              Elem.a [ Attr.href "/admin/users" ] [ Text.raw "Back to user list" ] ]

    // Internal API handlers - demonstrate path-less groups with PreCondition<'T>-only routes
    let internalMetrics (adminId: AdminId) : HttpHandler =
        let (AdminId aid) = adminId

        Response.ofJson
            {| admin = aid.ToString()
               metrics = {| requests = 1234; errors = 5 |} |}

    let internalDeepHealth (adminId: AdminId) : HttpHandler =
        let (AdminId aid) = adminId

        Response.ofJson
            {| admin = aid.ToString()
               database = "ok"
               cache = "ok"
               queue = "ok" |}

    let internalClearCache (adminId: AdminId) : HttpHandler =
        let (AdminId aid) = adminId

        Response.ofJson
            {| admin = aid.ToString()
               cleared = true
               message = "Cache cleared" |}

    // Nested routes with params handlers
    let userItemList (userId: UserId) (adminId: AdminId option) : HttpHandler =
        let (UserId uid) = userId

        let adminStr =
            adminId
            |> Option.map (fun (AdminId aid) -> aid.ToString())
            |> Option.defaultValue "none"

        htmlResponse
            "User Items"
            [ Elem.h1 [] [ Text.raw "User Items" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "User ID: " ]
                    Elem.code [] [ Text.raw (uid.ToString()) ] ]
              Elem.p [] [ Elem.strong [] [ Text.raw "Admin ID: " ]; Elem.code [] [ Text.raw adminStr ] ]
              Elem.p [] [ Text.raw "This demonstrates nested routes with parent params: /nested-user/{userId}/items" ]
              Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]

    let userItemDetail (userId: UserId) (itemId: ItemId) (adminId: AdminId option) : HttpHandler =
        let (UserId uid) = userId
        let (ItemId iid) = itemId

        let adminStr =
            adminId
            |> Option.map (fun (AdminId aid) -> aid.ToString())
            |> Option.defaultValue "none"

        htmlResponse
            "Item Detail"
            [ Elem.h1 [] [ Text.raw "Item Detail" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "User ID: " ]
                    Elem.code [] [ Text.raw (uid.ToString()) ] ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "Item ID: " ]
                    Elem.code [] [ Text.raw (iid.ToString()) ] ]
              Elem.p [] [ Elem.strong [] [ Text.raw "Admin ID: " ]; Elem.code [] [ Text.raw adminStr ] ]
              Elem.p [] [ Text.raw "This demonstrates /nested-user/{userId}/items/{itemId}" ]
              Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]

    let userItemPublic (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        htmlResponse
            "Public Items"
            [ Elem.h1 [] [ Text.raw "Public Items (No Admin Required)" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "User ID: " ]
                    Elem.code [] [ Text.raw (uid.ToString()) ] ]
              Elem.p
                  []
                  [ Text.raw "This route uses "
                    Elem.code [] [ Text.raw "[<SkipAllPreconditions>]" ]
                    Text.raw " to skip all optional preconditions." ]
              Elem.p [] [ Text.raw "No X-Admin-Id header required!" ]
              Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]

    let userItemLimited (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        htmlResponse
            "Limited Items"
            [ Elem.h1 [] [ Text.raw "Limited Items (Specific Skip)" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "User ID: " ]
                    Elem.code [] [ Text.raw (uid.ToString()) ] ]
              Elem.p
                  []
                  [ Text.raw "This route uses "
                    Elem.code [] [ Text.raw "[<SkipPrecondition(typeof<AdminId>)>]" ]
                    Text.raw " to skip only OptionalPreCondition<AdminId>." ]
              Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]

    let userDashboardOverview (userId: UserId) (adminId: AdminId option) : HttpHandler =
        let (UserId uid) = userId

        let adminStr =
            adminId
            |> Option.map (fun (AdminId aid) -> aid.ToString())
            |> Option.defaultValue "none"

        htmlResponse
            "User Dashboard"
            [ Elem.h1 [] [ Text.raw "User Dashboard" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "User ID: " ]
                    Elem.code [] [ Text.raw (uid.ToString()) ] ]
              Elem.p [] [ Elem.strong [] [ Text.raw "Admin ID: " ]; Elem.code [] [ Text.raw adminStr ] ]
              Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]

    let userDashboardSettings (userId: UserId) (adminId: AdminId option) : HttpHandler =
        let (UserId uid) = userId

        let adminStr =
            adminId
            |> Option.map (fun (AdminId aid) -> aid.ToString())
            |> Option.defaultValue "none"

        htmlResponse
            "User Settings"
            [ Elem.h1 [] [ Text.raw "User Settings" ]
              Elem.p
                  []
                  [ Elem.strong [] [ Text.raw "User ID: " ]
                    Elem.code [] [ Text.raw (uid.ToString()) ] ]
              Elem.p [] [ Elem.strong [] [ Text.raw "Admin ID: " ]; Elem.code [] [ Text.raw adminStr ] ]
              Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]

// =============================================================================
// 8. Route Handlers (routes are already hydrated by top-level extractor)
// =============================================================================
// With recursive hydration, all nested routes are hydrated at the top level.
// Handlers just pattern match on already-hydrated values - no per-route hydration needed!

let handlePost (route: PostRoute) : HttpHandler =
    match route with
    | PostRoute.List page -> Handlers.listPosts page
    | PostRoute.Detail postId -> Handlers.getPost postId
    | PostRoute.Create(PreCondition userId) -> Handlers.createPost userId
    | PostRoute.Delete(PreCondition userId, postId) -> Handlers.deletePost (userId, postId)
    | PostRoute.Patch(PreCondition userId, postId) -> Handlers.patchPost (userId, postId)
    | PostRoute.Search query -> Handlers.searchPosts query

let handleUser (route: UserRoute) : HttpHandler =
    match route with
    | UserRoute.Profile userId -> Handlers.getProfile userId
    | UserRoute.Update userId -> Handlers.updateUser userId
    | UserRoute.Me -> Handlers.currentUser

let handleApi (route: ApiRoute) : HttpHandler =
    match route with
    | ApiRoute.Webhook -> Handlers.webhook
    | ApiRoute.Status -> Handlers.apiStatus
    | ApiRoute.Article slug -> Handlers.article slug

let handleAdmin (route: AdminRoute) : HttpHandler =
    match route with
    | AdminRoute.Dashboard(PreCondition adminId) -> Handlers.adminDashboard adminId
    | AdminRoute.Users(PreCondition adminId) -> Handlers.adminUserList adminId
    | AdminRoute.UserDetail(PreCondition adminId, userId) -> Handlers.adminUserDetail (adminId, userId)
    | AdminRoute.BanUser(PreCondition adminId, PreCondition userId, targetId) ->
        Handlers.banUser (adminId, userId, targetId)

let handleInternal (route: InternalApiRoute) : HttpHandler =
    match route with
    | InternalApiRoute.Metrics(PreCondition adminId) -> Handlers.internalMetrics adminId
    | InternalApiRoute.DeepHealth(PreCondition adminId) -> Handlers.internalDeepHealth adminId
    | InternalApiRoute.ClearCache(PreCondition adminId) -> Handlers.internalClearCache adminId

// Nested user handlers - all routes are already hydrated, just dispatch
let handleUserItem (userId: UserId) (adminIdOpt: AdminId option) (route: UserItemRoute) : HttpHandler =
    match route with
    | UserItemRoute.List -> Handlers.userItemList userId adminIdOpt
    | UserItemRoute.Show itemId -> Handlers.userItemDetail userId itemId adminIdOpt
    | UserItemRoute.Public -> Handlers.userItemPublic userId
    | UserItemRoute.Limited -> Handlers.userItemLimited userId

let handleUserDashboard (userId: UserId) (adminIdOpt: AdminId option) (route: UserDashboardRoute) : HttpHandler =
    match route with
    | UserDashboardRoute.Overview -> Handlers.userDashboardOverview userId adminIdOpt
    | UserDashboardRoute.Settings -> Handlers.userDashboardSettings userId adminIdOpt

let handleNestedUser (userId: UserId) (adminIdOpt: AdminId option) (route: NestedUserRoute) : HttpHandler =
    match route with
    | NestedUserRoute.Items itemRoute -> handleUserItem userId adminIdOpt itemRoute
    | NestedUserRoute.Dashboard dashRoute -> handleUserDashboard userId adminIdOpt dashRoute

let handleUserWithParams (route: UserWithParamsRoute) : HttpHandler =
    match route with
    | UserWithParamsRoute.NestedUser(userId, OverridablePreCondition adminId, nestedRoute) ->
        // Convert sentinel Guid.Empty to None, actual values to Some
        // (Skipped OverridablePreCondition uses default value, which for AdminId is AdminId(Guid.Empty))
        let adminIdOpt =
            match adminId with
            | AdminId id when id = Guid.Empty -> None
            | AdminId _ -> Some adminId

        handleNestedUser userId adminIdOpt nestedRoute

// =============================================================================
// 9. Top-level Route Handler with Single Hydration Point
// =============================================================================
// Handle already-hydrated routes - dispatches to nested handlers

let handleRoute (route: Route) : HttpHandler =
    match route with
    | Route.Root -> Handlers.home
    | Route.Health -> Handlers.health
    | Route.Posts postRoute -> handlePost postRoute
    | Route.Users userRoute -> handleUser userRoute
    | Route.Api apiRoute -> handleApi apiRoute
    | Route.Admin adminRoute -> handleAdmin adminRoute
    | Route.Internal internalRoute -> handleInternal internalRoute
    | Route.UserWithParams userWithParamsRoute -> handleUserWithParams userWithParamsRoute

// =============================================================================
// 10. Convert to Falco Endpoints
// =============================================================================
// Route.endpoints does everything:
// - Validates route structure at startup
// - Extracts route/query params automatically
// - Runs precondition extractors (auth, validation)
// - Hydrates nested routes recursively
// - Converts errors to HTTP responses

let endpoints = Route.endpoints endpointConfig handleRoute

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
