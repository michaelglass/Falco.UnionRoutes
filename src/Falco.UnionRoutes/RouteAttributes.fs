namespace Falco.UnionRoutes

open System

/// HTTP methods for route attributes
type RouteMethod =
    | Get = 0
    | Post = 1
    | Put = 2
    | Delete = 3
    | Patch = 4

/// HTTP method discriminated union (for use in route handlers)
[<RequireQualifiedAccess>]
type HttpMethod =
    | Get
    | Post
    | Put
    | Delete
    | Patch

/// Attribute to specify route metadata on union cases.
///
/// Example usage:
/// ```fsharp
/// type UserRoute =
///     | [<Route(RouteMethod.Get, Path = "users")>] List
///     | [<Route(RouteMethod.Get, Path = "users/{id}")>] Detail of id: Guid
///     | [<Route(RouteMethod.Post, Path = "users")>] Create
/// ```
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type RouteAttribute(method: RouteMethod) =
    inherit Attribute()

    /// The HTTP method for this route
    member _.Method = method

    /// Path pattern for this route segment.
    /// - Use "" for wrapper cases that just nest other routes
    /// - Use explicit paths like "users" or "users/{id}"
    /// - Path parameters use {paramName} syntax
    member val Path: string = null with get, set
