namespace Falco.UnionRoutes

open System

/// <summary>HTTP methods for route attributes.</summary>
/// <remarks>Used with <see cref="RouteAttribute"/> to specify the HTTP method for a route case.</remarks>
type RouteMethod =
    /// <summary>HTTP GET method.</summary>
    | Get = 0
    /// <summary>HTTP POST method.</summary>
    | Post = 1
    /// <summary>HTTP PUT method.</summary>
    | Put = 2
    /// <summary>HTTP DELETE method.</summary>
    | Delete = 3
    /// <summary>HTTP PATCH method.</summary>
    | Patch = 4
    /// <summary>Matches any HTTP method.</summary>
    | Any = 5

/// <summary>HTTP method discriminated union for use in route handlers.</summary>
[<RequireQualifiedAccess>]
type HttpMethod =
    /// <summary>HTTP GET method.</summary>
    | Get
    /// <summary>HTTP POST method.</summary>
    | Post
    /// <summary>HTTP PUT method.</summary>
    | Put
    /// <summary>HTTP DELETE method.</summary>
    | Delete
    /// <summary>HTTP PATCH method.</summary>
    | Patch
    /// <summary>Matches any HTTP method.</summary>
    | Any

/// <summary>Attribute to specify route metadata on union cases.</summary>
/// <remarks>
/// Use this attribute to override the default HTTP method or path for a route case.
/// Convention-based routing works without attributes for common patterns.
/// </remarks>
/// <example>
/// <code>
/// type UserRoute =
///     | [&lt;Route(RouteMethod.Get, Path = "users")&gt;] List
///     | [&lt;Route(RouteMethod.Get, Path = "users/{id}")&gt;] Detail of id: Guid
///     | [&lt;Route(RouteMethod.Post, Path = "users")&gt;] Create
/// </code>
/// </example>
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type RouteAttribute(method: RouteMethod) =
    inherit Attribute()

    /// <summary>Creates a RouteAttribute with GET as the default method.</summary>
    new() = RouteAttribute(RouteMethod.Get)

    /// <summary>Gets the HTTP method for this route.</summary>
    /// <returns>The HTTP method specified for this route case.</returns>
    member _.Method = method

    /// <summary>Gets or sets the path pattern for this route segment.</summary>
    /// <value>
    /// The path pattern string. Use <c>""</c> for wrapper cases that just nest other routes,
    /// or explicit paths like <c>"users"</c> or <c>"users/{id}"</c>.
    /// Path parameters use <c>{paramName}</c> syntax.
    /// </value>
    member val Path: string = null with get, set

/// <summary>Skip all <c>OptPre&lt;'T&gt;</c> preconditions for this route case.</summary>
/// <remarks>
/// <para><c>Pre&lt;'T&gt;</c> (strict preconditions) are NOT affected - they always run.</para>
/// <para>When skipped, the handler should ignore the value with <c>_</c> pattern.</para>
/// </remarks>
/// <example>
/// <code>
/// type UserItemRoute =
///     | List                                  // inherits parent OptPre preconditions
///     | [&lt;SkipAllPreconditions&gt;] Public       // skips all OptPre, handler uses _ pattern
/// </code>
/// </example>
/// <seealso cref="SkipPreconditionAttribute"/>
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type SkipAllPreconditionsAttribute() =
    inherit Attribute()

/// <summary>Skip a specific <c>OptPre&lt;'T&gt;</c> precondition type for this route case.</summary>
/// <remarks>
/// <para><c>Pre&lt;'T&gt;</c> (strict preconditions) are NOT affected - they always run.</para>
/// <para>Can be applied multiple times to skip multiple precondition types.</para>
/// </remarks>
/// <example>
/// <code>
/// type AdminRoute =
///     | Dashboard                                         // requires all
///     | [&lt;SkipPrecondition(typeof&lt;AdminId&gt;)&gt;] Profile     // skips OptPre&lt;AdminId&gt; only
/// </code>
/// </example>
/// <seealso cref="SkipAllPreconditionsAttribute"/>
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = true)>]
type SkipPreconditionAttribute(preconditionType: Type) =
    inherit Attribute()

    /// <summary>Gets the inner type of the <c>OptPre&lt;'T&gt;</c> to skip.</summary>
    /// <value>The type to skip, e.g., <c>typeof&lt;AdminId&gt;</c> for <c>OptPre&lt;AdminId&gt;</c>.</value>
    member _.PreconditionType = preconditionType
