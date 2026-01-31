# Falco.UnionRoutes

Coming from Haskell, I missed Servant-style routing. So I built this library to add Union Type-based routing to [Falco](https://github.com/pimbrouwers/Falco).

## Features

- **Type-safe Routes**: Define routes as discriminated unions with attributes
- **Route Reflection**: Automatically extract routes from DUs via reflection
- **Pipeline Composition**: Railway-oriented programming for handler preconditions
- **Error Handling**: Short-circuit on first error with custom error handling

## Installation

```bash
dotnet add package Falco.UnionRoutes
```

## Quick Start

See the complete working example at [`examples/ExampleApp/Program.fs`](examples/ExampleApp/Program.fs).

### 1. Define Domain Types

Use single-case unions for type-safe IDs:

```fsharp
// examples/ExampleApp/Program.fs lines 24-25
type UserId = UserId of Guid
type PostId = PostId of Guid
```

### 2. Define Routes as Discriminated Unions

Use `[<Route>]` attributes on union cases. Use `Path = ""` for wrapper cases that group nested routes:

```fsharp
// examples/ExampleApp/Program.fs lines 34-56

// Convention-based routes (no attributes needed for common patterns)
type PostRoute =
    | List                   // GET /posts (List → empty path)
    | Detail of id: Guid     // GET /posts/{id} (path from field name)
    | Create                 // POST /posts (Create → POST + empty path)
    | Delete of id: Guid     // DELETE /posts/{id} (Delete → DELETE)
    | Patch of id: Guid      // PATCH /posts/{id} (Patch → PATCH)

// Explicit attributes for custom paths or overriding conventions
type UserRoute =
    | [<Route(Path = "{userId}")>] Profile of userId: Guid  // Custom param name
    | [<Route(RouteMethod.Put, Path = "{id}")>] Update of id: Guid  // PUT instead of convention
    | [<Route(Path = "me")>] Me  // Custom static path

// Mix of conventions and explicit attributes
type ApiRoute =
    | [<Route(RouteMethod.Any, Path = "webhook")>] Webhook  // Any HTTP method
    | [<Route(Path = "v2/status")>] Status  // Custom nested path

type Route =
    | Root                   // GET / (Root → empty path)
    | Posts of PostRoute     // nested under /posts
    | Users of UserRoute     // nested under /users
    | Api of ApiRoute        // nested under /api
    | Health                 // GET /health
```

### 3. Define Error Type and Response Handler

```fsharp
// examples/ExampleApp/Program.fs lines 53-57
type AppError =
    | NotAuthenticated
    | Forbidden
    | NotFound of string
    | BadRequest of string

// examples/ExampleApp/Program.fs lines 91-106
let toErrorResponse (error: AppError) : HttpHandler =
    match error with
    | NotAuthenticated -> Response.withStatusCode 401 >> Response.ofPlainText "Unauthorized"
    | Forbidden -> Response.withStatusCode 403 >> Response.ofPlainText "Forbidden"
    | NotFound msg -> Response.withStatusCode 404 >> Response.ofPlainText msg
    | BadRequest msg -> Response.withStatusCode 400 >> Response.ofPlainText msg
```

### 4. Define Pipelines

Pipelines extract and validate data from HTTP requests, short-circuiting on the first error:

```fsharp
// examples/ExampleApp/Program.fs lines 115-130
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
```

### 5. Map Routes to Handlers

Use `Pipeline.run` for single pipelines, compose with `<&>` for multiple:

```fsharp
// examples/ExampleApp/Program.fs lines 314-342
let routeHandler (route: Route) : HttpHandler =
    match route with
    | Route.Root -> Handlers.home
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
    | Route.Posts(PostRoute.Patch _) ->
        Pipeline.run toErrorResponse (requireAuth <&> requirePostId) Handlers.patchPost

    // User routes with different pipeline patterns
    | Route.Users(UserRoute.Profile _) -> Pipeline.run toErrorResponse requireUserId Handlers.getProfile
    | Route.Users(UserRoute.Update _) -> Pipeline.run toErrorResponse requireUserId Handlers.updateUser
    | Route.Users UserRoute.Me -> Handlers.currentUser

    // API routes - simple handlers
    | Route.Api ApiRoute.Webhook -> Handlers.webhook
    | Route.Api ApiRoute.Status -> Handlers.apiStatus
```

### 6. Convert to Falco Endpoints

```fsharp
// examples/ExampleApp/Program.fs line 298
let endpoints = RouteReflection.endpoints routeHandler
```

## API Reference

### Route Attributes

The `[<Route>]` attribute is optional. All union cases are routes by default with conventions:

```fsharp
// No attribute needed - conventions apply
| List                        // GET, empty path (collection root)
| Detail of id: Guid          // GET, path "{id}" (from field)
| Create                      // POST, empty path
| Delete of id: Guid          // DELETE, path "{id}"
| Patch of id: Guid           // PATCH, path "{id}"
| Health                      // GET, path "health" (kebab-case)

// Explicit attributes override conventions
| [<Route(RouteMethod.Put, Path = "{id}")>] Update of id: Guid
| [<Route(RouteMethod.Any, Path = "webhook")>] Webhook
| [<Route(Path = "v2/status")>] Status
```

**Path inference:**
- Fields like `of id: Guid` → `{id}`
- Multiple fields `of a: Guid * b: Guid` → `{a}/{b}`
- No fields → kebab-case from name (`MyRoute` → `my-route`)
- Special names: `Root`, `List`, `Create`, `Show` → empty path `""`

**Method inference:**
- `Create` → POST
- `Delete` → DELETE
- `Patch` → PATCH
- All others → GET

**Available methods:** Get, Post, Put, Delete, Patch, Any

### Route Reflection

```fsharp
// Get route metadata (path pattern with {param} placeholders)
RouteReflection.routeInfo route        // { Method: HttpMethod; Path: string }
RouteReflection.tryRouteInfo route     // RouteInfo option
RouteReflection.routeTuple route       // HttpMethod * string

// Type-safe links (substitutes actual values into path)
RouteReflection.link (Posts (Detail myGuid))  // "/posts/11111111-..."

// Enumerate all routes (parameterized routes get default values)
RouteReflection.allRoutes<Route>()     // Route list

// Falco integration
RouteReflection.endpoints routeHandler // HttpEndpoint list (generates all endpoints)
RouteReflection.toFalcoMethod method   // get/post/put/delete/patch/any function

// Utilities
RouteReflection.toKebabCase "MyRoute"  // "my-route"
```

### Pipeline Type

```fsharp
type Pipeline<'T, 'TError> = HttpContext -> Result<'T, 'TError>
```

### Pipeline Composition

```fsharp
// Combine two pipelines into a tuple
let combined = pipeline1 <&> pipeline2  // Pipeline<'a * 'b, 'e>

// Transformations
Pipeline.map f pipeline                  // Pipeline<'b, 'e>
Pipeline.bind f pipeline                 // Pipeline<'b, 'e>
Pipeline.ignoreResult pipeline           // Pipeline<unit, 'e>

// Constructors
Pipeline.succeed value                   // Pipeline that always succeeds
Pipeline.fail error                      // Pipeline that always fails
```

### Built-in Route Parameter Pipelines

```fsharp
// Extract typed GUID from route
requireRouteId "id" UserId error         // Pipeline<UserId, 'e>
requireRouteIdWith "id" UserId errorFn   // Pipeline<UserId, 'e> (lazy error)

// Extract string from route
requireRouteStr "name" error             // Pipeline<string, 'e>
requireRouteStrWith "name" errorFn       // Pipeline<string, 'e>

// Extract int from route
requireRouteInt "page" error             // Pipeline<int, 'e>
```

### Pipeline Execution

```fsharp
// Run pipeline, converting errors to HTTP responses
Pipeline.run toErrorResponse pipeline handler  // HttpHandler
```

### Result Helpers

```fsharp
// Convert Option to Result
requireSome error optionValue            // Result<'T, 'E>
requireSomeWith errorFn optionValue      // Result<'T, 'E>
```

## Why Use This?

**Before (manual validation in each handler):**
```fsharp
let deletePost : HttpHandler = fun ctx -> task {
    // Auth check
    match getAuth ctx with
    | None -> return! Response.withStatusCode 401 >> Response.ofPlainText "Unauthorized" <| ctx
    | Some userId ->
        // Route param check
        match tryGetRouteGuid ctx "id" with
        | None -> return! Response.withStatusCode 400 >> Response.ofPlainText "Invalid ID" <| ctx
        | Some postId ->
            // Finally, the actual logic
            return! Response.ofJson {| deleted = postId |} ctx
}
```

**After (declarative pipelines):**
```fsharp
// examples/ExampleApp/Program.fs lines 289-290
| Route.Posts(PostRoute.Delete _) ->
    Pipeline.run toErrorResponse (requireAuth <&> requirePostId) Handlers.deletePost
```

## License

MIT
