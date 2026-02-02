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
    | Create (Pre userId) -> Response.ofJson (createPost userId)
```

**What you get:**
- Compiler enforces you handle every route
- `RouteReflection.link (Detail postId)` → `"/posts/abc-123"` (type-checked)
- Route/query params and auth automatically extracted based on field types

## Installation

```bash
dotnet add package Falco.UnionRoutes
```

## How It Works

Routes are discriminated unions. Field names become URL parameters:

```fsharp
type PostRoute =
    | List                                  // GET /posts
    | Detail of id: Guid                    // GET /posts/{id}
    | Create                                // POST /posts (convention: Create → POST)
    | Delete of id: Guid                    // DELETE /posts/{id} (convention: Delete → DELETE)
```

Special marker types change where values come from:

```fsharp
| Search of query: QueryParam<string>            // GET /posts/search?query=hello
| Search of q: QueryParam<string> option         // optional query param
| Create of PreCondition<UserId>                 // UserId from auth pipeline, not URL
| Edit of PreCondition<UserId> * id: Guid        // auth + route param
| Admin of OptionalPreCondition<AdminId> * data  // skippable precondition (child routes can opt out)
```

Single-case wrapper DUs are auto-unwrapped:

```fsharp
type PostId = PostId of Guid
| Detail of id: PostId                      // extracts Guid from URL, wraps in PostId
```

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

// 2. Create hydration (extracts params + runs auth)
let authPrecondition () = RouteHydration.forPre<UserId, AppError> requireAuth
let hydratePost = RouteHydration.create<PostRoute, AppError>
    [authPrecondition()] [] makeError combineErrors

// 3. Handle routes (compiler ensures exhaustive)
let handlePost route : HttpHandler =
    match route with
    | List page -> Response.ofJson (getPosts page)
    | Detail postId -> Response.ofJson (getPost postId)
    | Create (Pre userId) -> Response.ofJson (createPost userId)

// 4. Wire up
let postHandler route = Pipeline.run toErrorResponse (hydratePost route) handlePost
let routeHandler route =
    match route with
    | Home -> Response.ofPlainText "home"
    | Posts p -> postHandler p

let endpoints = RouteReflection.endpoints routeHandler
```

## Reference

See [`examples/ExampleApp/Program.fs`](examples/ExampleApp/Program.fs) for a complete working example.

### Route Conventions

**Routing behavior:**

| Case Definition                      | Path               | Notes                    |
|--------------------------------------|--------------------|--------------------------|
| `Health`                             | `/health`          | kebab-case from name     |
| `DigestView`                         | `/digest-view`     | kebab-case from name     |
| `Detail of id: Guid`                 | `/{id}`            | field name → path param  |
| `Edit of a: Guid * b: Guid`          | `/{a}/{b}`         | multiple path params     |
| `Posts of PostRoute`                 | `/posts/...`       | nested DU → path prefix  |
| `[<Route(Path = "")>] Api of ApiRoute` | `/...`           | path-less group          |

**Special case names:**

| Case Name | Method | Path | Notes                          |
|-----------|--------|------|--------------------------------|
| `Root`    | GET    | `/`  | empty path                     |
| `List`    | GET    | `/`  | empty path                     |
| `Create`  | POST   | `/`  | empty path, POST method        |
| `Delete`  | DELETE | —    | DELETE method (path from args) |
| `Patch`   | PATCH  | —    | PATCH method (path from args)  |

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

### Marker Types

| Type | Source | Example |
|------|--------|---------|
| `QueryParam<'T>` | Query string | `?page=2` |
| `QueryParam<'T> option` | Optional query | missing → `None` |
| `PreCondition<'T>` | Precondition pipeline | auth, validation (strict) |
| `OptionalPreCondition<'T>` | Skippable precondition | child routes can opt out |

### Preconditions

`PreCondition<'T>` fields are extracted from precondition pipelines (auth, validation, etc.), not from the URL:

```fsharp
type PostRoute =
    | List                                    // no auth required
    | Create of PreCondition<UserId>          // requires auth
    | Delete of PreCondition<UserId> * id: Guid  // auth + route param
```

**Skippable preconditions:** Use `OptionalPreCondition<'T>` when child routes can opt out:

```fsharp
type UserItemRoute =
    | List                                             // inherits preconditions
    | [<SkipAllPreconditions>] Public                  // skips all optional preconditions
    | [<SkipPrecondition(typeof<AdminId>)>] Limited    // skips specific one

type Route =
    | Users of userId: UserId * OptionalPreCondition<AdminId> * UserItemRoute
```

- `PreCondition<'T>` is strict - always runs, cannot be skipped
- `OptionalPreCondition<'T>` is skippable via attributes on child routes

### Nested Routes

Routes can be nested. When a case has both parameters AND a nested route, the case name becomes a path segment:

```fsharp
type ItemRoute =
    | List
    | Detail of itemId: Guid

type Route =
    | Items of ItemRoute                           // /items/...
    | UserItems of userId: Guid * ItemRoute        // /user-items/{userId}/...
```

### Route Validation

Validate routes at startup to catch configuration errors early (not at request time).

**At app startup:**

```fsharp
// Collect all preconditions
let allPreconditions =
    [ authPrecondition ()
      adminPrecondition ()
      optAdminPrecondition () ]

// Validate precondition coverage - fails fast if any Pre<T>/OptPre<T> is missing
do
    match RouteHydration.validatePreconditions<Route, AppError> allPreconditions with
    | Ok () -> ()
    | Error errors ->
        let errorMsg = errors |> String.concat "\n  - "
        failwith $"Route precondition validation failed:\n  - {errorMsg}"

// endpoints also validates route structure (paths, field names, etc.) automatically
let endpoints = RouteReflection.endpoints routeHandler
```

**In tests (recommended):**

```fsharp
[<Fact>]
let ``all routes are valid`` () =
    let preconditions =
        [ authPrecondition ()
          adminPrecondition ()
          optAdminPrecondition () ]

    // Full validation: route structure + precondition coverage
    let result = RouteHydration.validate<Route, AppError> preconditions
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
| All `Pre<T>` have preconditions | `validatePreconditions` (manual) |
| All `OptPre<T>` have preconditions | `validatePreconditions` (manual) |

### Key Functions

```fsharp
// Route reflection
RouteReflection.endpoints handler        // Generate Falco endpoints (validates structure)
RouteReflection.link route               // Type-safe URL: "/posts/abc-123"
RouteReflection.allRoutes<Route>()       // Enumerate all routes
RouteReflection.validateStructure<Route>() // Validate route structure only

// Route hydration
RouteHydration.create [preconditions] [extractors] makeError combineErrors
RouteHydration.forPre<'T,'E> pipeline    // Create strict precondition for Pre<'T>
RouteHydration.forOptPre<'T,'E> pipeline // Create skippable precondition for OptPre<'T>

// Validation
RouteHydration.validatePreconditions<Route, Error> preconditions  // Check precondition coverage
RouteHydration.validate<Route, Error> preconditions               // Full validation (for tests)

// Pipeline composition
Pipeline.run toError pipeline handler    // Execute with error handling
pipeline1 <&> pipeline2                  // Combine pipelines
```

## License

MIT
