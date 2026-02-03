<!-- sync:intro:start -->
# Falco.UnionRoutes

Define your routes as F# discriminated unions. Get exhaustive pattern matching, type-safe links, and automatic parameter extraction.

```fsharp
type PostRoute =
    | List of page: QueryParam<int> option
    | Detail of id: PostId
    | Create of PreCondition<UserId>

let handlePost route : HttpHandler =
    match route with
    | List page -> Response.ofJson (getPosts page)
    | Detail postId -> Response.ofJson (getPost postId)
    | Create (PreCondition userId) -> Response.ofJson (createPost userId)
```

**What you get:**
- Compiler enforces you handle every route
- `Route.link (Detail postId)` -> `"/posts/abc-123"` (type-checked)
- Route/query params and auth automatically extracted based on field types
<!-- sync:intro:end -->

## Installation

```bash
dotnet add package Falco.UnionRoutes
```

**[API Documentation](https://michaelglass.github.io/Falco.UnionRoutes/)**

<!-- sync:howitworks:start -->
## How It Works

Routes are discriminated unions. Field names become URL parameters:

```fsharp
type PostRoute =
    | List                                  // GET /posts
    | Detail of id: Guid                    // GET /posts/{id}
    | Create                                // POST /posts (convention: Create -> POST)
    | Delete of id: Guid                    // DELETE /posts/{id} (convention: Delete -> DELETE)
```

Special marker types change where values come from:

```fsharp
| Search of query: QueryParam<string>               // GET /posts/search?query=hello
| Search of q: QueryParam<string> option            // optional query param
| Create of PreCondition<UserId>                    // UserId from auth extractor, not URL
| Edit of PreCondition<UserId> * id: Guid           // auth + route param
| Admin of OverridablePreCondition<AdminId> * data  // skippable precondition (child routes can opt out)
```

Single-case wrapper DUs are auto-unwrapped:

```fsharp
type PostId = PostId of Guid
| Detail of id: PostId                      // extracts Guid from URL, wraps in PostId
```
<!-- sync:howitworks:end -->

<!-- sync:basicusage:start -->
## Basic Usage

```fsharp
// 1. Define routes
type Route =
    | Home
    | Posts of PostRoute

type PostRoute =
    | List of page: QueryParam<int> option
    | Detail of id: PostId
    | Create of PreCondition<UserId>

// 2. Create extractor (extracts params + runs auth)
let authPrecondition = Extractor.precondition<UserId, AppError> requireAuth
let hydratePost = Route.extractor<PostRoute, AppError>
    [authPrecondition] [] makeError combineErrors

// 3. Handle routes (compiler ensures exhaustive)
let handlePost route : HttpHandler =
    match route with
    | List page -> Response.ofJson (getPosts page)
    | Detail postId -> Response.ofJson (getPost postId)
    | Create (PreCondition userId) -> Response.ofJson (createPost userId)

// 4. Wire up
let postHandler route = Extraction.run toErrorResponse (hydratePost route) handlePost
let routeHandler route =
    match route with
    | Home -> Response.ofPlainText "home"
    | Posts p -> postHandler p

let endpoints = Route.endpoints routeHandler
```
<!-- sync:basicusage:end -->

## Reference

See [`examples/ExampleApp/Program.fs`](examples/ExampleApp/Program.fs) for a complete working example.

<!-- sync:conventions:start -->
### Route Conventions

**Routing behavior:**

| Case Definition                      | Path               | Notes                    |
|--------------------------------------|--------------------|--------------------------|
| `Health`                             | `/health`          | kebab-case from name     |
| `DigestView`                         | `/digest-view`     | kebab-case from name     |
| `Detail of id: Guid`                 | `/{id}`            | field name -> path param |
| `Edit of a: Guid * b: Guid`          | `/{a}/{b}`         | multiple path params     |
| `Posts of PostRoute`                 | `/posts/...`       | nested DU -> path prefix |
| `[<Route(Path = "")>] Api of ApiRoute` | `/...`           | path-less group          |

**RESTful case names (no case name prefix in path):**

| Case Name | Method | Path | Notes                          |
|-----------|--------|------|--------------------------------|
| `Root`    | GET    | `/`  | empty path                     |
| `List`    | GET    | `/`  | empty path                     |
| `Create`  | POST   | `/`  | empty path, POST method        |
| `Show`    | GET    | `/{params}` | param-only path           |
| `Edit`    | GET    | `/{params}` | param-only path           |
| `Delete`  | DELETE | `/{params}` | DELETE method             |
| `Patch`   | PATCH  | `/{params}` | PATCH method              |

**Override with attributes:**

```fsharp
[<Route(RouteMethod.Put)>]                           // just method
[<Route(Path = "custom/{id}")>]                      // just path
[<Route(RouteMethod.Put, Path = "custom/{id}")>]     // both
```

Available methods: `Get`, `Post`, `Put`, `Delete`, `Patch`, `Any`

**Path-less groups:**

Use `Path = ""` to group routes without adding a path segment:

```fsharp
type InternalRoute =
    | Metrics of PreCondition<AdminId>                         // GET /metrics (convention)
    | [<Route(Path = "health-deep")>] DeepHealth of PreCondition<AdminId>  // custom path

type Route =
    | Public of PublicRoute
    | [<Route(Path = "")>] Internal of InternalRoute  // no /internal prefix
```

Routes appear at `/metrics` and `/health-deep`, not `/internal/metrics`.
<!-- sync:conventions:end -->

<!-- sync:markertypes:start -->
### Marker Types

| Type | Source | Example |
|------|--------|---------|
| `QueryParam<'T>` | Query string | `?page=2` |
| `QueryParam<'T> option` | Optional query | missing -> `None` |
| `PreCondition<'T>` | Precondition extractor | auth, validation (strict) |
| `OverridablePreCondition<'T>` | Skippable precondition | child routes can opt out |
<!-- sync:markertypes:end -->

<!-- sync:preconditions:start -->
### Preconditions

`PreCondition<'T>` fields are extracted from precondition extractors (auth, validation, etc.), not from the URL:

```fsharp
type PostRoute =
    | List                                    // no auth required
    | Create of PreCondition<UserId>          // requires auth
    | Delete of PreCondition<UserId> * id: Guid  // auth + route param
```

**Skippable preconditions:** Use `OverridablePreCondition<'T>` when child routes can opt out:

```fsharp
type UserItemRoute =
    | List                                             // inherits preconditions
    | [<SkipAllPreconditions>] Public                  // skips all overridable preconditions
    | [<SkipPrecondition(typeof<AdminId>)>] Limited    // skips specific one

type Route =
    | Users of userId: UserId * OverridablePreCondition<AdminId> * UserItemRoute
```

- `PreCondition<'T>` is strict - always runs, cannot be skipped
- `OverridablePreCondition<'T>` is skippable via attributes on child routes
<!-- sync:preconditions:end -->

<!-- sync:nestedroutes:start -->
### Nested Routes

Routes can be nested. When a case has both parameters AND a nested route, the case name becomes a path segment:

```fsharp
type ItemRoute =
    | List
    | Show of itemId: Guid                         // "Show" convention: /{itemId}

type Route =
    | Items of ItemRoute                           // /items/...
    | UserItems of userId: Guid * ItemRoute        // /user-items/{userId}/...
```
<!-- sync:nestedroutes:end -->

<!-- sync:validation:start -->
### Route Validation

Validate routes at startup to catch configuration errors early (not at request time).

**At app startup:**

```fsharp
// Collect all preconditions
let allPreconditions =
    [ Extractor.precondition<UserId, AppError> requireAuth
      Extractor.precondition<AdminId, AppError> requireAdmin
      Extractor.overridablePrecondition<AdminId, AppError> requireAdmin ]

// Validate precondition coverage - fails fast if any precondition type is missing
do
    match Route.validatePreconditions<Route, AppError> allPreconditions with
    | Ok () -> ()
    | Error errors ->
        let errorMsg = errors |> String.concat "\n  - "
        failwith $"Route precondition validation failed:\n  - {errorMsg}"

// endpoints also validates route structure (paths, field names, etc.) automatically
let endpoints = Route.endpoints routeHandler
```

**In tests (recommended):**

```fsharp
[<Fact>]
let ``all routes are valid`` () =
    let preconditions =
        [ Extractor.precondition<UserId, AppError> requireAuth
          Extractor.precondition<AdminId, AppError> requireAdmin
          Extractor.overridablePrecondition<AdminId, AppError> requireAdmin ]

    // Full validation: route structure + precondition coverage
    let result = Route.validate<Route, AppError> preconditions
    Assert.Equal(Ok (), result)
```

**What gets validated:**

| Check | When |
|-------|------|
| Invalid path characters | `endpoints` (automatic) |
| Unbalanced braces in paths | `endpoints` (automatic) |
| Duplicate path params | `endpoints` (automatic) |
| Path params match field names | `endpoints` (automatic) |
| Multiple nested route unions | `endpoints` (automatic) |
| All `PreCondition<T>` have extractors | `validatePreconditions` (manual) |
| All `OverridablePreCondition<T>` have extractors | `validatePreconditions` (manual) |
<!-- sync:validation:end -->

<!-- sync:keyfunctions:start -->
### Key Functions

```fsharp
// Route module
Route.endpoints handler              // Generate Falco endpoints (validates structure)
Route.link route                     // Type-safe URL: "/posts/abc-123"
Route.allRoutes<Route>()             // Enumerate all routes
Route.validateStructure<Route>()     // Validate route structure only
Route.validatePreconditions<Route, Error> preconditions  // Check precondition coverage
Route.validate<Route, Error> preconditions               // Full validation (for tests)
Route.extractor [preconditions] [parsers] makeError combineErrors  // Create extraction function

// Extractor module
Extractor.precondition<UserId, Error> extractor           // For PreCondition<UserId> fields
Extractor.overridablePrecondition<AdminId, Error> extractor  // For OverridablePreCondition<AdminId> fields
Extractor.parser<Slug> parser                             // For custom types in route/query params

// Extraction module
Extraction.run toError extractor handler    // Execute with error handling
extractor1 <&> extractor2                   // Combine extractors
```
<!-- sync:keyfunctions:end -->

<!-- sync:license:start -->
## License

MIT
<!-- sync:license:end -->
