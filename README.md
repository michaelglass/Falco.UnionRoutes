# Falco.UnionRoutes

Define your routes as F# discriminated unions. Get exhaustive pattern matching, type-safe links, and automatic parameter extraction.

```fsharp
type PostRoute =
    | List of page: QueryParam<int> option
    | Detail of id: PostId
    | Create of PreCondition<UserId>

let handle route : HttpHandler =
    match route with
    | List page -> Response.ofJson (getPosts page)
    | Detail postId -> Response.ofJson (getPost postId)
    | Create (PreCondition userId) -> Response.ofJson (createPost userId)
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
| Create of PreCondition<UserId>                     // UserId from auth pipeline, not URL
| Edit of PreCondition<UserId> * id: Guid            // auth + route param
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
    | List
    | Detail of id: PostId
    | Create of PreCondition<UserId>

// 2. Create hydration (extracts params + runs auth)
let authPrecondition () = RouteHydration.forPre<UserId, AppError> requireAuth
let hydratePost = RouteHydration.create<PostRoute, AppError>
    [authPrecondition()] [] makeError combineErrors

// 3. Handle routes (compiler ensures exhaustive)
let handlePost route : HttpHandler =
    match route with
    | List -> Response.ofJson posts
    | Detail postId -> Response.ofJson (loadPost postId)
    | Create (PreCondition userId) -> Response.ofJson (createPost userId)

// 4. Wire up
let postHandler route = Pipeline.run toError (hydratePost route) handlePost
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

**Special case names:**

| Case Name | Method | Path | Notes                          |
|-----------|--------|------|--------------------------------|
| `Root`    | GET    | `/`  | empty path                     |
| `List`    | GET    | `/`  | empty path                     |
| `Create`  | POST   | `/`  | empty path, POST method        |
| `Delete`  | DELETE | —    | DELETE method (path from args) |
| `Patch`   | PATCH  | —    | PATCH method (path from args)  |

Override with `[<Route(RouteMethod.Put, Path = "custom/{id}")>]`.

### Marker Types

| Type | Source | Example |
|------|--------|---------|
| `QueryParam<'T>` | Query string | `?page=2` |
| `QueryParam<'T> option` | Optional query | missing → `None` |
| `PreCondition<'T>` | Precondition pipeline | auth, validation |

### Key Functions

```fsharp
RouteReflection.endpoints handler     // Generate Falco endpoints
RouteReflection.link route            // Type-safe URL: "/posts/abc-123"
RouteReflection.allRoutes<Route>()    // Enumerate all routes

RouteHydration.create [preconditions] [extractors] makeError combineErrors
RouteHydration.forPre<'T,'E> pipeline // Create precondition for PreCondition<'T>

Pipeline.run toError pipeline handler // Execute with error handling
pipeline1 <&> pipeline2               // Combine pipelines
```

## License

MIT
