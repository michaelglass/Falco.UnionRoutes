# Falco.UnionRoutes

Union type-based routing for [Falco](https://github.com/pimbrouwers/Falco), inspired by Haskell's Servant.

## Features

- **Type-safe routes**: Define routes as discriminated unions
- **Auto-extraction**: Route params, query strings, and auth preconditions extracted based on field types
- **Wrapper type support**: Single-case DUs like `type PostId = PostId of Guid` are auto-unwrapped
- **Custom extractors**: Extend with your own types
- **Error accumulation**: Collect all validation errors

## Installation

```bash
dotnet add package Falco.UnionRoutes
```

## Quick Start

See [`examples/ExampleApp/Program.fs`](examples/ExampleApp/Program.fs) for a complete working example.

### 1. Define Routes

```fsharp
type PostRoute =
    | List of page: Query<int> option   // GET /posts?page=2
    | Detail of id: PostId              // GET /posts/{id} - auto-unwraps PostId
    | Create of Pre<UserId>             // POST /posts - requires auth
    | Delete of Pre<UserId> * id: PostId

type Route =
    | Root                   // GET /
    | Posts of PostRoute     // nested under /posts
    | Health                 // GET /health
```

**Conventions:**
- `Create` -> POST, `Delete` -> DELETE, `Patch` -> PATCH, others -> GET
- Field names become path params: `of id: Guid` -> `{id}`
- Case names become paths: `Health` -> `/health`
- Use `[<Route(...)>]` to override

### 2. Define Error Handling

```fsharp
type AppError =
    | NotAuthenticated
    | BadRequest of string

let toErrorResponse error : HttpHandler =
    match error with
    | NotAuthenticated -> Response.withStatusCode 401 >> Response.ofPlainText "Unauthorized"
    | BadRequest msg -> Response.withStatusCode 400 >> Response.ofPlainText msg
```

### 3. Define Auth Pipeline

```fsharp
let requireAuth: Pipeline<UserId, AppError> =
    fun ctx ->
        match ctx.Request.Headers.TryGetValue("X-User-Id") with
        | true, values ->
            match Guid.TryParse(values.ToString()) with
            | true, guid -> Ok(UserId guid)
            | false, _ -> Error NotAuthenticated
        | false, _ -> Error NotAuthenticated
```

### 4. Create Route Hydration

```fsharp
let makeError msg = BadRequest msg
let combineErrors errors =
    match errors with
    | [ single ] -> single
    | multiple -> BadRequest(multiple |> List.map string |> String.concat "; ")

let authPrecondition () = RouteHydration.forPre<UserId, AppError> requireAuth

let hydratePost: PostRoute -> Pipeline<PostRoute, AppError> =
    RouteHydration.create<PostRoute, AppError> [ authPrecondition () ] [] makeError combineErrors
```

### 5. Map Routes to Handlers

```fsharp
// Shared data loading helpers (define once, use in handlers)
// let loadPost (PostId id) = db.Posts.Find(id)
// let loadUser (UserId id) = db.Users.Find(id)

let handlePost (route: PostRoute) : HttpHandler =
    match route with
    | PostRoute.List page ->
        // page is Query<int> option - None if not provided
        Handlers.listPosts page
    | PostRoute.Detail postId ->
        // postId is already a PostId (auto-unwrapped from Guid)
        // let post = loadPost postId
        Handlers.getPost postId
    | PostRoute.Create(Pre userId) ->
        // userId extracted from Pre<UserId> via pattern match
        Handlers.createPost userId
    | PostRoute.Delete(Pre userId, postId) ->
        // Both auth and route param available
        // let user = loadUser userId
        // let post = loadPost postId
        Handlers.deletePost (userId, postId)

let postHandler route = Pipeline.run toErrorResponse (hydratePost route) handlePost
```

### 6. Generate Endpoints

```fsharp
let routeHandler (route: Route) : HttpHandler =
    match route with
    | Route.Root -> Handlers.home
    | Route.Health -> Handlers.health
    | Route.Posts postRoute -> postHandler postRoute

let endpoints = RouteReflection.endpoints routeHandler
```

## Route Attributes

Override conventions with `[<Route>]`:

```fsharp
| [<Route(RouteMethod.Put, Path = "{id}")>] Update of id: Guid
| [<Route(Path = "v2/status")>] Status
```

## Query Parameters

```fsharp
| Search of query: Query<string>           // required: /search?query=hello
| List of page: Query<int> option          // optional: /posts or /posts?page=2
```

## Multiple Preconditions

```fsharp
type AdminRoute =
    | Dashboard of Pre<AdminId>
    | BanUser of Pre<AdminId> * Pre<UserId>  // both required

let hydrateAdmin = RouteHydration.create<AdminRoute, AppError>
    [ adminPrecondition (); userPrecondition () ]
    []
    makeError
    combineErrors
```

## Custom Extractors

For types beyond primitives and wrapper DUs:

```fsharp
type Slug = Slug of string

let slugExtractor: TypeExtractor =
    fun fieldName fieldType ctx ->
        if fieldType = typeof<Slug> then
            let value = (Request.getRoute ctx).GetString fieldName
            if String.IsNullOrEmpty(value) then Some(Error $"Missing: {fieldName}")
            else Some(Ok(box (Slug value)))
        else None

let hydrate = RouteHydration.create<MyRoute, AppError> [] [slugExtractor] makeError combineErrors
```

## API Reference

### RouteReflection

```fsharp
RouteReflection.routeInfo route       // { Method; Path } with {param} placeholders
RouteReflection.link route            // Concrete URL: "/posts/abc-123"
RouteReflection.allRoutes<Route>()    // Enumerate all routes
RouteReflection.endpoints handler     // Generate Falco endpoints
```

### Pipeline

```fsharp
type Pipeline<'T, 'E> = HttpContext -> Result<'T, 'E>

pipeline1 <&> pipeline2              // Combine into tuple
Pipeline.map f pipeline              // Transform success value
Pipeline.run toError pipeline handler // Execute with error handling
```

## License

MIT
