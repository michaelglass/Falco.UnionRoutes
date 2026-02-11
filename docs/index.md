<!-- sync:intro -->
# Falco.UnionRoutes

Define your routes as F# discriminated unions. Get exhaustive pattern matching, type-safe links, and automatic parameter extraction. Inspired by Haskell's [Servant](https://docs.servant.dev/) library.

```fsharp
type PostRoute =
    | List of page: QueryParam<int> option           // GET /posts?page=1
    | Detail of id: PostId                           // GET /posts/{id}
    | Create of JsonBody<PostInput> * PreCondition<UserId>  // POST /posts (JSON body + auth)

let handlePost route : HttpHandler =
    match route with
    | List page -> Response.ofJson (getPosts page)
    | Detail postId -> Response.ofJson (getPost postId)
    | Create (JsonBody input, PreCondition userId) -> Response.ofJson (createPost userId input)
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
| Create of JsonBody<PostInput> * PreCondition<UserId>  // JSON body + auth
| Submit of FormBody<LoginInput>                    // form-encoded body
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
    | [<Route(Path = "")>] Admin of AdminRoute

type PostInput = { Title: string; Body: string }

type PostRoute =
    | List of page: QueryParam<int> option              // GET /posts?page=1
    | Detail of id: PostId                              // GET /posts/{id}
    | Create of JsonBody<PostInput> * PreCondition<UserId>  // POST /posts (JSON body + auth)

type AdminRoute =
    | Dashboard of PreCondition<AdminId>      // GET /dashboard

// 2. Configure extraction — extractors are async (HttpContext -> Task<Result<'T, 'E>>)
let requireAuth : Extractor<UserId, AppError> = fun ctx ->
    Task.FromResult(
        match ctx.User.FindFirst(ClaimTypes.NameIdentifier) with
        | null -> Error NotAuthenticated
        | claim -> Ok (UserId (Guid.Parse claim.Value)))

let requireAdmin : Extractor<AdminId, AppError> = fun ctx ->
    Task.FromResult(
        if ctx.User.IsInRole("Admin") then
            match ctx.User.FindFirst(ClaimTypes.NameIdentifier) with
            | null -> Error NotAuthenticated
            | claim -> Ok (AdminId (Guid.Parse claim.Value))
        else Error (Forbidden "Admin role required"))

let config: EndpointConfig<AppError> = {
    Preconditions =
        [ Extractor.precondition<UserId, AppError> requireAuth
          Extractor.precondition<AdminId, AppError> requireAdmin ]
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
    | Create (JsonBody input, PreCondition userId) -> Response.ofJson (createPost userId input)

let handleRoute route : HttpHandler =
    match route with
    | Home -> Response.ofPlainText "home"
    | Posts p -> handlePost p
    | Admin (Dashboard (PreCondition adminId)) -> Response.ofPlainText "admin"

// 4. Generate endpoints - extraction happens automatically
let endpoints = Route.endpoints config handleRoute
```
<!-- sync:basicusage:end -->

## Reference

See the [Example App](https://github.com/michaelglass/Falco.UnionRoutes/tree/main/examples/ExampleApp) for a complete working example. Run it with `mise run example`.

<!-- sync:conventions -->
### Route Conventions

**Routing behavior:**

| Case Definition                      | Path               | Notes                    |
|--------------------------------------|--------------------|--------------------------|
| `Health`                             | `/health`          | kebab-case from name     |
| `DigestView`                         | `/digest-view`     | kebab-case from name     |
| `Detail of id: Guid`                 | `/{id:guid}`       | field name -> path param + type constraint |
| `ByPage of page: int`                | `/{page:int}`      | int -> `:int` constraint |
| `Edit of a: Guid * b: Guid`          | `/{a:guid}/{b:guid}` | multiple path params   |
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
[<Route(Constraints = [| RouteConstraint.Alpha |], MinLength = 3, MaxLength = 50)>]  // constraints
[<Route(MinValue = 1, MaxValue = 100)>]              // integer range
[<Route(Pattern = @"^\d{3}-\d{4}$")>]               // regex pattern
```

**Implicit type constraints** — applied automatically based on field types:

| Field Type | Constraint | Example Path |
|-----------|------------|-------------|
| `Guid` | `:guid` | `{id:guid}` |
| `int` | `:int` | `{page:int}` |
| `int64` | `:long` | `{id:long}` |
| `bool` | `:bool` | `{enabled:bool}` |
| `string` | (none) | `{name}` |
| Single-case DU (e.g. `PostId of Guid`) | inner type's constraint | `{id:guid}` |

Implicit and explicit constraints combine: a `Guid` field with `[<Route(Constraints = [| Required |])>]` produces `{id:guid:required}`.

**Parser constraints** — applied by `Route.endpoints` when custom parsers declare constraints:

```fsharp
Extractor.constrainedParser<Slug> [| RouteConstraint.Alpha |] parseFn  // adds :alpha
Extractor.typedParser<bool, ToggleState> parseFn                       // adds :bool (from input type)
```

Parser constraints are applied at endpoint registration time. `Route.info` and `Route.link` reflect only type-based constraints since they don't require `EndpointConfig`.
<!-- sync:conventions:end -->

<!-- sync:markertypes -->
### Marker Types

| Type | Source | Example |
|------|--------|---------|
| `QueryParam<'T>` | Query string | `?page=2` |
| `QueryParam<'T> option` | Optional query | missing -> `None` |
| `PreCondition<'T>` | Precondition extractor | auth, validation (strict) |
| `OverridablePreCondition<'T>` | Skippable precondition | child routes can opt out |
| `Returns<'T>` | Response type metadata | `Route.respond returns value` |
| `JsonBody<'T>` | JSON request body | deserialized via `System.Text.Json` |
| `FormBody<'T>` | Form-encoded body | form fields mapped to record |
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
    | List of page: QueryParam<int> option                 // GET    /posts?page=1
    | Create of JsonBody<PostInput> * PreCondition<UserId> // POST   /posts (JSON body + auth)
    | Search of query: QueryParam<string>                  // GET    /posts/search?query=hello
    | Member of id: Guid * PostDetailRoute                 //        /posts/{id}/...
```

`Member` produces a param-only path (no case-name prefix). `Show`/`Delete`/`Patch` collapse to the same path with different methods. `Edit` produces `/edit`.
<!-- sync:nestedroutes:end -->

<!-- sync:validation -->
### Route Validation

`Route.endpoints` automatically validates at startup and will fail fast with descriptive errors. `Route.validate` combines all checks for use in tests.

**Structure errors** (`Route.validateStructure`):

| Error | Example | Message |
|-------|---------|---------|
| Invalid path characters | `[<Route(Path = "hello world")>]` | Invalid characters in path |
| Unbalanced braces | `[<Route(Path = "users/{id")>]` | Unbalanced braces in path |
| Duplicate path params | `[<Route(Path = "{id}/sub/{id}")>]` | Duplicate path parameters |
| Param/field mismatch | `[<Route(Path = "{userId}")>] Profile of id: Guid` | Path params not found in fields |
| Multiple nested unions | `Both of ChildA * ChildB` | Case has 2 nested route unions (max 1) |
| Multiple body fields | `Bad of JsonBody<A> * FormBody<B>` | At most 1 body field per case |
| Body + nested union | `Bad of JsonBody<A> * ChildRoute` | Body field cannot coexist with nested route |

**Uniqueness errors** (checked by `Route.validate` and `Route.endpoints`):

| Error | Example | Message |
|-------|---------|---------|
| Duplicate routes | `ById of id: Guid` + `BySlug of slug: string` both at `GET /items/{_}` | Duplicate route: 'ById' and 'BySlug' both resolve to... |
| Ambiguous routes | `GET /{cat}/new` vs `GET /posts/{action}` (neither is more specific) | Ambiguous routes: ... overlap with no clear specificity winner |

**Precondition errors** (`Route.validatePreconditions`):

| Error | Example | Message |
|-------|---------|---------|
| Missing extractor | `PreCondition<UserId>` field with no registered extractor | Missing preconditions for: PreCondition\<UserId\> |

Routes with overlapping patterns are automatically sorted by specificity (`/posts/new` before `/posts/{id}`).

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
Route.respond returns value          // Type-safe JSON response via Returns<'T>
Route.link route                     // Type-safe URL: "/posts/abc-123"
Route.info route                     // RouteInfo with Method and Path
Route.allRoutes<Route>()             // Enumerate all routes
Route.validateStructure<Route>()     // Validate path structure only
Route.validatePreconditions<Route, Error> preconditions  // Check precondition coverage
Route.validate<Route, Error> preconditions               // Full validation (for tests)

// EndpointConfig record (passed to Route.endpoints)
{ Preconditions = [...]              // Auth/validation extractors
  Parsers = [...]                    // Custom type parsers
  MakeError = fun msg -> ...         // String -> error type
  CombineErrors = fun errors -> ...  // Combine multiple errors
  ToErrorResponse = fun e -> ... }   // Error -> HTTP response

// Extractor module - create extractors for EndpointConfig
// Extractors are async: Extractor<'T,'E> = HttpContext -> Task<Result<'T,'E>>
Extractor.precondition<UserId, Error> extractFn              // For PreCondition<UserId> fields
Extractor.preconditionSync<UserId, Error> syncExtractFn      // Sync convenience wrapper
Extractor.overridablePrecondition<AdminId, Error> extractFn  // For OverridablePreCondition<AdminId>
Extractor.overridablePreconditionSync<AdminId, Error> syncFn // Sync convenience wrapper
Extractor.parser<Slug> parseFn                               // For custom types (string input)
Extractor.constrainedParser<Slug> [| Alpha |] parseFn        // String parser + route constraints
Extractor.typedParser<bool, Toggle> parseFn                  // Typed parser (pre-parsed input)
```
<!-- sync:keyfunctions:end -->

<!-- sync:license -->
## License

MIT
<!-- sync:license:end -->
