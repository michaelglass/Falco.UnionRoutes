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

### Nested Routes with Parent Params

When a route case has both parameters AND a nested route, the case name is included in the path:

```fsharp
type UserItemRoute =
    | List
    | Detail of itemId: ItemId
    | [<SkipAllPreconditions>] Public  // skips optional preconditions

type NestedUserRoute =
    | Items of UserItemRoute

type UserWithParamsRoute =
    NestedUser of userId: UserId * OptionalPreCondition<AdminId> * NestedUserRoute

// Handler receives parent params explicitly, passes to child
let handleUserItem (userId: UserId) (adminId: AdminId option) (route: UserItemRoute) : HttpHandler =
    match route with
    | List -> Handlers.userItemList userId adminId
    | Detail itemId -> Handlers.userItemDetail userId itemId adminId
    | Public -> Handlers.userItemPublic userId  // adminId skipped

let handleUserWithParams (route: UserWithParamsRoute) : HttpHandler =
    match route with
    | NestedUser (userId, OptPre adminId, nestedRoute) ->
        handleNestedUser userId (Some adminId) nestedRoute
```

### Marker Types

| Type | Source | Example |
|------|--------|---------|
| `QueryParam<'T>` | Query string | `?page=2` |
| `QueryParam<'T> option` | Optional query | missing → `None` |
| `PreCondition<'T>` | Precondition pipeline | auth, validation (strict) |
| `OptionalPreCondition<'T>` | Skippable precondition | child routes can opt out |

### Skippable Preconditions

Use `OptionalPreCondition<'T>` for preconditions that child routes can skip:

```fsharp
type UserItemRoute =
    | List                                             // inherits parent preconditions
    | [<SkipAllPreconditions>] Public                  // skips all optional preconditions
    | [<SkipPrecondition(typeof<AdminId>)>] Limited    // skips only OptionalPreCondition<AdminId>

type UserWithParamsRoute =
    NestedUser of userId: UserId * OptionalPreCondition<AdminId> * NestedUserRoute

// Handler uses _ pattern for skipped preconditions
let handleUserWithParams (route: UserWithParamsRoute) : HttpHandler =
    match route with
    | NestedUser (userId, OptPre adminId, Items List) ->
        // adminId verified and available
        handleList userId (Some adminId)
    | NestedUser (userId, _, Items Public) ->
        // adminId skipped - use _ pattern
        handlePublic userId
```

Key points:
- `PreCondition<'T>` is strict - always runs, cannot be skipped
- `OptionalPreCondition<'T>` is skippable - child routes can opt out
- `[<SkipAllPreconditions>]` skips all optional preconditions
- `[<SkipPrecondition(typeof<T>)>]` skips specific `OptionalPreCondition<T>`

### Key Functions

```fsharp
RouteReflection.endpoints handler     // Generate Falco endpoints
RouteReflection.link route            // Type-safe URL: "/posts/abc-123"
RouteReflection.allRoutes<Route>()    // Enumerate all routes

RouteHydration.create [preconditions] [extractors] makeError combineErrors
RouteHydration.forPre<'T,'E> pipeline    // Create strict precondition for Pre<'T>
RouteHydration.forOptPre<'T,'E> pipeline // Create skippable precondition for OptPre<'T>

Pipeline.run toError pipeline handler // Execute with error handling
pipeline1 <&> pipeline2               // Combine pipelines
```

## License

MIT
