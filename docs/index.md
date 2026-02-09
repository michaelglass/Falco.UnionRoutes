<!-- sync:intro -->
# Falco.UnionRoutes

Define your routes as F# discriminated unions. Get exhaustive pattern matching, type-safe links, and automatic parameter extraction. Inspired by Haskell's [Servant](https://docs.servant.dev/) library.

```fsharp
type PostRoute =
    | List of page: QueryParam<int> option    // GET /posts?page=1
    | Detail of id: PostId                    // GET /posts/{id}
    | Create of PreCondition<UserId>          // POST /posts

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

**[API Reference](reference/index.html)**

<!-- sync:howitworks -->
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

<!-- sync:basicusage -->
## Basic Usage

```fsharp
// 1. Define routes
type Route =
    | Home                                    // GET /home
    | Posts of PostRoute                      // /posts/...

type PostRoute =
    | List of page: QueryParam<int> option    // GET /posts?page=1
    | Detail of id: PostId                    // GET /posts/{id}
    | Create of PreCondition<UserId>          // POST /posts

// 2. Configure extraction (preconditions, parsers, error handling)
let config: EndpointConfig<AppError> = {
    Preconditions = [ Extractor.precondition<UserId, AppError> requireAuth ]
    Parsers = []
    MakeError = fun msg -> BadRequest msg
    CombineErrors = fun errors -> errors |> List.head
    ToErrorResponse = fun e -> Response.withStatusCode 400 >> Response.ofPlainText (string e)
}

// 3. Handle routes (compiler ensures exhaustive, routes already hydrated)
let handlePost route : HttpHandler =
    match route with
    | List page -> Response.ofJson (getPosts page)
    | Detail postId -> Response.ofJson (getPost postId)
    | Create (PreCondition userId) -> Response.ofJson (createPost userId)

let handleRoute route : HttpHandler =
    match route with
    | Home -> Response.ofPlainText "home"
    | Posts p -> handlePost p

// 4. Generate endpoints - extraction happens automatically
let endpoints = Route.endpoints config handleRoute
```
<!-- sync:basicusage:end -->

## Reference

See the [Example App](https://github.com/michaelglass/Falco.UnionRoutes/tree/main/examples/ExampleApp) for a complete working example.

<!-- sync:conventions -->
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
| `Member`  | GET    | `/{params}` | param-only path (alias for Show) |
| `Edit`    | GET    | `/edit`     | produces path segment     |
| `Delete`  | DELETE | `/{params}` | DELETE method             |
| `Patch`   | PATCH  | `/{params}` | PATCH method              |

**Override with attributes:**

```fsharp
[<Route(RouteMethod.Put)>]                           // just method
[<Route(Path = "custom/{id}")>]                      // just path
[<Route(RouteMethod.Put, Path = "custom/{id}")>]     // both
```
<!-- sync:conventions:end -->

<!-- sync:markertypes -->
### Marker Types

| Type | Source | Example |
|------|--------|---------|
| `QueryParam<'T>` | Query string | `?page=2` |
| `QueryParam<'T> option` | Optional query | missing -> `None` |
| `PreCondition<'T>` | Precondition extractor | auth, validation (strict) |
| `OverridablePreCondition<'T>` | Skippable precondition | child routes can opt out |
<!-- sync:markertypes:end -->

<!-- sync:preconditions -->
### Preconditions

`OverridablePreCondition<'T>` lets child routes opt out with attributes:

```fsharp
type ItemRoute =
    | List                                             // inherits preconditions
    | [<SkipAllPreconditions>] Public                  // skips all overridable preconditions
    | [<SkipPrecondition(typeof<UserId>)>] Limited     // skips specific one

type Route =
    | Items of userId: UserId * OverridablePreCondition<UserId> * ItemRoute
```

- `PreCondition<'T>` — strict, always runs, cannot be skipped
- `OverridablePreCondition<'T>` — skippable via `[<SkipAllPreconditions>]` or `[<SkipPrecondition(typeof<T>)>]`
<!-- sync:preconditions:end -->

<!-- sync:nestedroutes -->
### Nested Routes

```fsharp
type PostDetailRoute =
    | Show                                         // GET    /posts/{id}
    | Edit                                         // GET    /posts/{id}/edit
    | Delete                                       // DELETE /posts/{id}
    | Patch                                        // PATCH  /posts/{id}

type PostRoute =
    | List of page: QueryParam<int> option         // GET    /posts?page=1
    | Create of PreCondition<UserId>               // POST   /posts
    | Search of query: QueryParam<string>          // GET    /posts/search?query=hello
    | Member of id: Guid * PostDetailRoute         //        /posts/{id}/...
```

`Member` produces a param-only path (no case-name prefix). `Show`/`Delete`/`Patch` collapse to the same path with different methods. `Edit` produces `/edit`.
<!-- sync:nestedroutes:end -->

<!-- sync:validation -->
### Route Validation

`Route.endpoints` automatically validates route structure at startup. To also check precondition coverage, add a test:

```fsharp
[<Fact>]
let ``all routes are valid`` () =
    let result = Route.validate<Route, AppError> config.Preconditions
    Assert.Equal(Ok (), result)
```
<!-- sync:validation:end -->

<!-- sync:keyfunctions -->
### Key Functions

```fsharp
// Route module
Route.endpoints config handler       // Generate endpoints with extraction (main entry point)
Route.link route                     // Type-safe URL: "/posts/abc-123"
Route.info route                     // RouteInfo with Method and Path
Route.allRoutes<Route>()             // Enumerate all routes
Route.validateStructure<Route>()     // Validate route structure only
Route.validatePreconditions<Route, Error> preconditions  // Check precondition coverage
Route.validate<Route, Error> preconditions               // Full validation (for tests)

// EndpointConfig record (passed to Route.endpoints)
{ Preconditions = [...]              // Auth/validation extractors
  Parsers = [...]                    // Custom type parsers
  MakeError = fun msg -> ...         // String -> error type
  CombineErrors = fun errors -> ...  // Combine multiple errors
  ToErrorResponse = fun e -> ... }   // Error -> HTTP response

// Extractor module - create extractors for EndpointConfig
Extractor.precondition<UserId, Error> extractFn           // For PreCondition<UserId> fields
Extractor.overridablePrecondition<AdminId, Error> extractFn  // For OverridablePreCondition<AdminId>
Extractor.parser<Slug> parseFn                            // For custom types in route/query params
```
<!-- sync:keyfunctions:end -->

<!-- sync:license -->
## License

MIT
<!-- sync:license:end -->
