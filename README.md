# Falco.UnionRoutes

Coming from Haskell, I missed Servant-style routing. So I built this library to add Union Type-based routing to [Falco](https://github.com/pimbrouwers/Falco).

## Features

- **Type-safe Routes**: Define routes as discriminated unions with attributes
- **Route Reflection**: Automatically extract routes from DUs via reflection
- **Route Hydration**: Automatically extract route params and auth based on field types
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

Route fields define what each route needs - both route parameters AND auth requirements:

```fsharp
// Convention-based routes - fields define requirements
type PostRoute =
    | List                        // GET /posts - no params
    | Detail of id: Guid          // GET /posts/{id} - extracts id from route
    | Create of UserId            // POST /posts - requires auth (UserId from pipeline)
    | Delete of UserId * id: Guid // DELETE /posts/{id} - auth + route param
    | Patch of UserId * id: Guid  // PATCH /posts/{id} - auth + route param

// Explicit attributes for custom paths or overriding conventions
type UserRoute =
    | [<Route(Path = "{userId}")>] Profile of userId: Guid
    | [<Route(RouteMethod.Put, Path = "{id}")>] Update of id: Guid
    | [<Route(Path = "me")>] Me

type Route =
    | Root                   // GET /
    | Posts of PostRoute     // nested under /posts
    | Users of UserRoute     // nested under /users
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

### 4. Define Auth Pipeline

```fsharp
/// Extract user from auth header (simplified example)
let requireAuth: Pipeline<UserId, AppError> =
    fun ctx ->
        match ctx.Request.Headers.TryGetValue("X-User-Id") with
        | true, values ->
            match Guid.TryParse(values.ToString()) with
            | true, guid -> Ok(UserId guid)
            | false, _ -> Error NotAuthenticated
        | false, _ -> Error NotAuthenticated
```

### 5. Create Route Hydration

`RouteHydration.create` automatically extracts fields based on their types:
- Fields matching the auth type (`UserId`) use the auth pipeline
- `Guid` fields are extracted from route params by field name
- `string` and `int` fields work similarly
- All errors are accumulated and combined via the `makeErrors` function

```fsharp
/// Combine validation errors into a single BadRequest
let combineErrors (msgs: string list) = BadRequest(String.concat "; " msgs)

// Hydrate PostRoute - extracts auth and route params automatically
let hydratePost: PostRoute -> Pipeline<PostRoute, AppError> =
    RouteHydration.create<PostRoute, UserId, AppError> requireAuth combineErrors
```

### 6. Map Routes to Handlers

The handler receives a fully hydrated route with all values populated:

```fsharp
// Handler receives hydrated route - exhaustive pattern matching!
let handlePost (route: PostRoute) : HttpHandler =
    match route with
    | PostRoute.List -> Handlers.listPosts
    | PostRoute.Detail id -> Handlers.getPost (PostId id)
    | PostRoute.Create userId -> Handlers.createPost userId
    | PostRoute.Delete(userId, id) -> Handlers.deletePost (userId, PostId id)
    | PostRoute.Patch(userId, id) -> Handlers.patchPost (userId, PostId id)

// Combined: hydrate then handle
let postHandler (route: PostRoute) : HttpHandler =
    Pipeline.run toErrorResponse (hydratePost route) handlePost

// Top-level router delegates to postHandler
let routeHandler (route: Route) : HttpHandler =
    match route with
    | Route.Root -> Handlers.home
    | Route.Health -> Handlers.health
    | Route.Posts postRoute -> postHandler postRoute
    | Route.Users(UserRoute.Profile _) -> ...
```

### 7. Convert to Falco Endpoints

```fsharp
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

### Route Hydration

Automatically extract route parameters and auth based on field types:

```fsharp
// Create a hydrator for a route type
// - 'Auth type fields use the auth pipeline (errors preserve original type)
// - Guid fields extract from route params by field name
// - string/int fields work similarly
// - Single-case DU wrappers (PostId of Guid) are auto-detected
// - Query<'T> option returns None when missing instead of error
// - Query<'T> extracts from query string instead of route
// - Extraction errors are accumulated and combined via makeErrors
RouteHydration.create<'Route, 'Auth, 'Error> authPipeline makeErrors
    // Returns: 'Route -> Pipeline<'Route, 'Error>

// For routes without auth
RouteHydration.createNoAuth<'Route, 'Error> makeErrors

// With custom type extractors
RouteHydration.createWith<'Route, 'Auth, 'Error> [customExtractor] authPipeline makeErrors
RouteHydration.createNoAuthWith<'Route, 'Error> [customExtractor] makeErrors
```

Example:
```fsharp
type PostRoute =
    | List of page: Query<int> option // optional query param - /posts?page=2
    | Detail of id: PostId            // extracts Guid, wraps as PostId
    | Create of UserId                // uses auth pipeline
    | Delete of UserId * id: PostId   // auth + route param
    | Search of query: Query<string>  // required query param - /search?query=...

/// Combine extraction errors into a single BadRequest
let combineErrors (msgs: string list) = BadRequest(String.concat "; " msgs)

let hydratePost = RouteHydration.create<PostRoute, UserId, AppError> requireAuth combineErrors

// Usage: hydrate route, then handle
let postHandler route = Pipeline.run toError (hydratePost route) handlePost
```

### Query Parameters

```fsharp
// Query<'T> - extract from query string instead of route params
| Search of query: Query<string>             // /search?query=hello (required)

// Query<'T> option - optional query parameter
| List of page: Query<int> option            // /posts or /posts?page=2

// Mix route params and query params
| Detail of id: Guid * sort: Query<string> option  // /items/{id}?sort=date
```

### Custom Type Extractors

Extend hydration with custom types:

```fsharp
type Slug = Slug of string

/// Custom extractor - returns Some if it handles the type, None to defer
/// Uses string errors (accumulated with other extraction errors)
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
            None  // Can't handle, try next extractor

// Use with createWith or createNoAuthWith
let combineErrors msgs = BadRequest(String.concat "; " msgs)
let hydrateApi = RouteHydration.createNoAuthWith [slugExtractor] combineErrors
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

**After (route hydration):**
```fsharp
// Route fields declare what's needed
type PostRoute = Delete of UserId * id: Guid

// Hydration extracts everything automatically
// Auth errors preserve type (NotAuthenticated), extraction errors get combined
let combineErrors msgs = BadRequest(String.concat "; " msgs)
let hydratePost = RouteHydration.create<PostRoute, UserId, AppError> requireAuth combineErrors

// Handler receives fully populated values - exhaustive!
let handlePost route =
    match route with
    | PostRoute.Delete(userId, id) -> Handlers.deletePost (userId, PostId id)
    | ...

// Combined
let postHandler route = Pipeline.run toError (hydratePost route) handlePost
```

## License

MIT
