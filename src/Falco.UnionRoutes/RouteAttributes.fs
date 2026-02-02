namespace Falco.UnionRoutes

open System

/// HTTP methods for route attributes
type RouteMethod =
    | Get = 0
    | Post = 1
    | Put = 2
    | Delete = 3
    | Patch = 4
    | Any = 5

/// HTTP method discriminated union (for use in route handlers)
[<RequireQualifiedAccess>]
type HttpMethod =
    | Get
    | Post
    | Put
    | Delete
    | Patch
    | Any

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

    /// Secondary constructor that defaults to Get
    new() = RouteAttribute(RouteMethod.Get)

    /// The HTTP method for this route
    member _.Method = method

    /// Path pattern for this route segment.
    /// - Use "" for wrapper cases that just nest other routes
    /// - Use explicit paths like "users" or "users/{id}"
    /// - Path parameters use {paramName} syntax
    member val Path: string = null with get, set

/// Skip all OptPre preconditions for this route case.
/// Pre<'T> (strict preconditions) are NOT affected - they always run.
///
/// Example:
/// ```fsharp
/// type UserItemRoute =
///     | List                                  // inherits parent OptPre preconditions
///     | [<SkipAllPreconditions>] Public       // skips all OptPre, handler uses _ pattern
/// ```
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type SkipAllPreconditionsAttribute() =
    inherit Attribute()

/// Skip a specific OptPre precondition type for this route case.
/// Pre<'T> (strict preconditions) are NOT affected - they always run.
/// Can be applied multiple times to skip multiple precondition types.
///
/// Example:
/// ```fsharp
/// type AdminRoute =
///     | Dashboard                                         // requires all
///     | [<SkipPrecondition(typeof<AdminId>)>] Profile     // skips OptPre<AdminId> only
/// ```
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = true)>]
type SkipPreconditionAttribute(preconditionType: Type) =
    inherit Attribute()

    /// The inner type of the OptPre<'T> to skip (e.g., typeof<AdminId> for OptPre<AdminId>)
    member _.PreconditionType = preconditionType
