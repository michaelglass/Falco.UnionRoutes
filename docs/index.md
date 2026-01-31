# Falco.UnionRoutes

Type-safe routing with discriminated unions for [Falco](https://github.com/pimbrouwers/Falco).

## Features

- **Convention-based routing**: Define routes as discriminated unions with sensible defaults
- **Type-safe links**: Generate URLs from route values with `RouteReflection.link`
- **Pipeline composition**: Railway-oriented programming for handler preconditions
- **RESTful conventions**: `Create` → POST, `Delete` → DELETE, `Patch` → PATCH

## Quick Example

```fsharp
type PostRoute =
    | List                   // GET /posts
    | Detail of id: Guid     // GET /posts/{id}
    | Create                 // POST /posts
    | Delete of id: Guid     // DELETE /posts/{id}

type Route =
    | Root
    | Posts of PostRoute
    | Health

let endpoints = RouteReflection.endpoints routeHandler
```

## Documentation

- [API Reference](reference/index.html)
- [Example App](https://github.com/michaelglass/Falco.UnionRoutes/tree/main/examples/ExampleApp)

## Installation

```bash
dotnet add package Falco.UnionRoutes
```
