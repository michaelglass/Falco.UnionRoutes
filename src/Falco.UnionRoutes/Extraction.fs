namespace Falco.UnionRoutes

open System
open Falco
open Microsoft.AspNetCore.Http

// =========================================================================
// Route Field Marker Types (at namespace level for easy access)
// =========================================================================

/// <summary>Marks a route field as coming from the query string instead of route parameters.</summary>
/// <typeparam name="T">The type of the query parameter value.</typeparam>
/// <remarks>
/// <para>Use <c>QueryParam&lt;'T&gt; option</c> for optional query parameters that return None when missing.</para>
/// <para>Required query parameters (without option) return an error when missing.</para>
/// </remarks>
/// <example>
/// <code>
/// type PostRoute =
///     | Search of query: QueryParam&lt;string&gt;        // GET /posts/search?query=... (required)
///     | List of page: QueryParam&lt;int&gt; option       // GET /posts?page=2 (optional, defaults to None)
/// </code>
/// </example>
type QueryParam<'T> = QueryParam of 'T

/// <summary>Marks a route field as coming from a precondition extractor (auth, validation, etc.)
/// rather than from route or query parameters.</summary>
/// <typeparam name="T">The type of value provided by the precondition.</typeparam>
/// <remarks>
/// <para><c>PreCondition&lt;'T&gt;</c> is STRICT - it always runs and cannot be skipped by child routes.</para>
/// <para>Use <see cref="T:Falco.UnionRoutes.OverridablePreCondition`1"/> when child routes need to skip the precondition.</para>
/// </remarks>
/// <example>
/// <code>
/// type PostRoute =
///     | List                                       // No auth required
///     | Create of PreCondition&lt;UserId&gt;             // Requires authenticated user
///     | Delete of PreCondition&lt;UserId&gt; * id: Guid  // Auth + route param
/// </code>
/// </example>
/// <seealso cref="T:Falco.UnionRoutes.OverridablePreCondition`1"/>
type PreCondition<'T> = PreCondition of 'T

/// <summary>Marks a route field as a precondition that child routes can skip.</summary>
/// <typeparam name="T">The type of value provided by the precondition.</typeparam>
/// <remarks>
/// <para>Use this ONLY when you need child routes to optionally skip the precondition.
/// For most cases, use <see cref="T:Falco.UnionRoutes.PreCondition`1"/> instead.</para>
/// <para>Child routes skip with <c>[&lt;SkipAllPreconditions&gt;]</c> or <c>[&lt;SkipPrecondition(typeof&lt;T&gt;)&gt;]</c>.</para>
/// <para>When skipped, the field contains a default value - use <c>_</c> pattern to ignore it.</para>
/// </remarks>
/// <example>
/// <code>
/// type ItemRoute =
///     | List                                  // Inherits parent's OverridablePreCondition
///     | [&lt;SkipAllPreconditions&gt;] Public       // Skips it - no auth required
///
/// type Route =
///     | Items of OverridablePreCondition&lt;UserId&gt; * ItemRoute
///
/// // In handler, use _ to ignore skipped preconditions:
/// match route with
/// | Items (_, Public) -> handlePublic ()
/// | Items (OverridablePreCondition userId, List) -> handleList userId
/// </code>
/// </example>
/// <seealso cref="T:Falco.UnionRoutes.PreCondition`1"/>
type OverridablePreCondition<'T> = OverridablePreCondition of 'T

// =========================================================================
// Precondition Skip Attributes (at namespace level for easy access)
// =========================================================================

/// <summary>Skip all <c>OverridablePreCondition&lt;'T&gt;</c> preconditions for this route case.</summary>
/// <remarks>
/// <para><c>PreCondition&lt;'T&gt;</c> (strict preconditions) are NOT affected - they always run.</para>
/// <para>When skipped, the handler should ignore the value with <c>_</c> pattern.</para>
/// </remarks>
/// <example>
/// <code>
/// type UserItemRoute =
///     | List                                  // inherits parent OverridablePreCondition preconditions
///     | [&lt;SkipAllPreconditions&gt;] Public       // skips all OverridablePreCondition, handler uses _ pattern
/// </code>
/// </example>
/// <seealso cref="T:Falco.UnionRoutes.SkipPreconditionAttribute"/>
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type SkipAllPreconditionsAttribute() =
    inherit Attribute()

/// <summary>Skip a specific <c>OverridablePreCondition&lt;'T&gt;</c> precondition type for this route case.</summary>
/// <remarks>
/// <para><c>PreCondition&lt;'T&gt;</c> (strict preconditions) are NOT affected - they always run.</para>
/// <para>Can be applied multiple times to skip multiple precondition types.</para>
/// </remarks>
/// <example>
/// <code>
/// type AdminRoute =
///     | Dashboard                                         // requires all
///     | [&lt;SkipPrecondition(typeof&lt;AdminId&gt;)&gt;] Profile     // skips OverridablePreCondition&lt;AdminId&gt; only
/// </code>
/// </example>
/// <seealso cref="T:Falco.UnionRoutes.SkipAllPreconditionsAttribute"/>
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = true)>]
type SkipPreconditionAttribute(preconditionType: Type) =
    inherit Attribute()

    /// <summary>Gets the inner type of the <c>OverridablePreCondition&lt;'T&gt;</c> to skip.</summary>
    /// <value>The type to skip, e.g., <c>typeof&lt;AdminId&gt;</c> for <c>OverridablePreCondition&lt;AdminId&gt;</c>.</value>
    member _.PreconditionType = preconditionType

// =========================================================================
// Extraction Module
// =========================================================================

/// <summary>Core types and functions for extracting values from HTTP requests.</summary>
/// <remarks>
/// <para>This module provides the building blocks for type-safe route extraction:</para>
/// <list type="bullet">
///   <item><description><c>Parser&lt;'T&gt;</c> - converts strings to typed values (for route/query params)</description></item>
///   <item><description><c>Extractor&lt;'T,'E&gt;</c> - extracts values from HTTP context (for auth, headers, etc.)</description></item>
///   <item><description><c>Extractor.parser</c> - registers a parser for custom types</description></item>
///   <item><description><c>Extractor.precondition</c> - registers an extractor for precondition fields</description></item>
/// </list>
/// </remarks>
[<AutoOpen>]
module Extraction =

    // =========================================================================
    // Core Function Types
    // =========================================================================

    /// <summary>Converts a string to a typed value. Used for route and query parameters.</summary>
    /// <typeparam name="T">The type to parse into.</typeparam>
    /// <example>
    /// <code>
    /// let slugParser: Parser&lt;Slug&gt; = fun s -> Ok (Slug s)
    /// let intParser: Parser&lt;int&gt; = fun s ->
    ///     match Int32.TryParse s with
    ///     | true, i -> Ok i
    ///     | _ -> Error "Invalid integer"
    /// </code>
    /// </example>
    type Parser<'T> = string -> Result<'T, string>

    /// <summary>Extracts a value from HTTP context. Used for authentication, authorization,
    /// reading headers, cookies, or any value that requires request context.</summary>
    /// <typeparam name="T">The type to extract.</typeparam>
    /// <typeparam name="E">The error type on failure.</typeparam>
    /// <example>
    /// <code>
    /// let requireAuth: Extractor&lt;UserId, AppError&gt; = fun ctx ->
    ///     match ctx.Request.Headers.TryGetValue("X-User-Id") with
    ///     | true, values ->
    ///         match Guid.TryParse(values.ToString()) with
    ///         | true, guid -> Ok (UserId guid)
    ///         | _ -> Error NotAuthenticated
    ///     | _ -> Error NotAuthenticated
    /// </code>
    /// </example>
    type Extractor<'T, 'E> = HttpContext -> Result<'T, 'E>

    // =========================================================================
    // Extractor Configuration Types
    // =========================================================================

    /// <summary>Registers a parser for a custom type so Route.extractor knows how to
    /// convert route/query string values into that type.</summary>
    /// <remarks>
    /// <para>Create with <c>Extractor.parser</c>. Only needed for custom types -
    /// built-in types (Guid, string, int, int64, bool) and single-case DU wrappers
    /// are handled automatically.</para>
    /// </remarks>
    /// <example>
    /// <code>
    /// // Register parser for Slug type
    /// let slugParser = Extractor.parser (fun s -> Ok (Slug s))
    ///
    /// // Use with Route.extractor
    /// let extract = Route.extractor&lt;MyRoute, AppError&gt;
    ///     []              // no preconditions
    ///     [ slugParser ]  // custom parsers
    ///     makeError
    ///     combineErrors
    /// </code>
    /// </example>
    type FieldParser =
        {
            /// The type this parser handles (e.g., typeof<Slug>)
            ForType: Type
            /// Parse a string into the target type
            Parse: string -> Result<obj, string>
        }

    /// <summary>Registers an extractor for PreCondition or OverridablePreCondition fields
    /// so Route.extractor knows how to populate them from HTTP context.</summary>
    /// <typeparam name="E">The error type returned when extraction fails.</typeparam>
    /// <remarks>
    /// <para>Create with <c>Extractor.precondition</c> or <c>Extractor.overridablePrecondition</c>.</para>
    /// <para>Each PreCondition&lt;T&gt; or OverridablePreCondition&lt;T&gt; type used in routes
    /// must have a corresponding extractor registered.</para>
    /// </remarks>
    /// <example>
    /// <code>
    /// // Register extractor for PreCondition&lt;UserId&gt;
    /// let authExtractor = Extractor.precondition&lt;UserId, AppError&gt; (fun ctx ->
    ///     match getAuthCookie ctx with
    ///     | Some id -> Ok id
    ///     | None -> Error NotAuthenticated)
    ///
    /// // Use with Route.extractor
    /// let extract = Route.extractor&lt;MyRoute, AppError&gt;
    ///     [ authExtractor ]  // precondition extractors
    ///     []                 // no custom parsers
    ///     makeError
    ///     combineErrors
    /// </code>
    /// </example>
    type PreconditionExtractor<'E> =
        {
            /// The marker type this extracts (e.g., typeof<PreCondition<UserId>>)
            ForType: Type
            /// Extract the value from HTTP context
            Extract: HttpContext -> Result<obj, 'E>
        }

    // =========================================================================
    // Extractor Module - Factory Functions
    // =========================================================================

    /// <summary>Factory functions for creating extractors to use with Route.extractor.</summary>
    /// <remarks>
    /// <para>Route.extractor needs to know how to populate each field in your route types:</para>
    /// <list type="bullet">
    ///   <item><description>Built-in types (Guid, string, int, bool) - automatic</description></item>
    ///   <item><description>Single-case DU wrappers (e.g., PostId of Guid) - automatic</description></item>
    ///   <item><description>Custom types - use <c>Extractor.parser</c></description></item>
    ///   <item><description>PreCondition fields - use <c>Extractor.precondition</c></description></item>
    ///   <item><description>OverridablePreCondition fields - use <c>Extractor.overridablePrecondition</c></description></item>
    /// </list>
    /// </remarks>
    /// <example>
    /// <code>
    /// let extract = Route.extractor&lt;PostRoute, AppError&gt;
    ///     [ Extractor.precondition&lt;UserId, _&gt; requireAuth
    ///       Extractor.overridablePrecondition&lt;AdminId, _&gt; requireAdmin ]
    ///     [ Extractor.parser&lt;Slug&gt; (fun s -> Ok (Slug s)) ]
    ///     (fun msg -> BadRequest msg)
    ///     (fun errors -> errors |> List.head)
    /// </code>
    /// </example>
    [<RequireQualifiedAccess>]
    module Extractor =
        /// <summary>Registers an extractor for <c>PreCondition&lt;'T&gt;</c> fields.</summary>
        /// <typeparam name="T">The inner type (e.g., UserId in PreCondition&lt;UserId&gt;).</typeparam>
        /// <typeparam name="E">The error type.</typeparam>
        /// <param name="extractor">Function that extracts the value from HTTP context.</param>
        /// <returns>A PreconditionExtractor to pass to Route.extractor.</returns>
        let precondition<'T, 'E> (extractor: Extractor<'T, 'E>) : PreconditionExtractor<'E> =
            { ForType = typeof<PreCondition<'T>>
              Extract = fun ctx -> extractor ctx |> Result.map (fun v -> box (PreCondition v)) }

        /// <summary>Registers an extractor for <c>OverridablePreCondition&lt;'T&gt;</c> fields.</summary>
        /// <typeparam name="T">The inner type (e.g., AdminId in OverridablePreCondition&lt;AdminId&gt;).</typeparam>
        /// <typeparam name="E">The error type.</typeparam>
        /// <param name="extractor">Function that extracts the value from HTTP context.</param>
        /// <returns>A PreconditionExtractor to pass to Route.extractor.</returns>
        /// <remarks>
        /// <para>Use this when child routes may skip the precondition with
        /// <c>[&lt;SkipAllPreconditions&gt;]</c> or <c>[&lt;SkipPrecondition(typeof&lt;T&gt;)&gt;]</c>.</para>
        /// </remarks>
        let overridablePrecondition<'T, 'E> (extractor: Extractor<'T, 'E>) : PreconditionExtractor<'E> =
            { ForType = typeof<OverridablePreCondition<'T>>
              Extract = fun ctx -> extractor ctx |> Result.map (fun v -> box (OverridablePreCondition v)) }

        /// <summary>Registers a parser for a custom type used in route or query parameters.</summary>
        /// <typeparam name="T">The type to parse (e.g., Slug, CustomId).</typeparam>
        /// <param name="parser">Function that parses a string into the type.</param>
        /// <returns>A FieldParser to pass to Route.extractor.</returns>
        /// <remarks>
        /// <para>Only needed for custom types. Built-in types and single-case DU wrappers
        /// around built-in types are handled automatically.</para>
        /// </remarks>
        let parser<'T> (parser: Parser<'T>) : FieldParser =
            { ForType = typeof<'T>
              Parse = fun s -> parser s |> Result.map box }

    // =========================================================================
    // Internal Helpers
    // =========================================================================

    /// Convert Option to Result with specified error
    let internal requireSome error opt =
        match opt with
        | Some v -> Ok v
        | None -> Error error

    /// Convert Option to Result, applying a function to create the error
    let internal requireSomeWith errorFn opt =
        match opt with
        | Some v -> Ok v
        | None -> Error(errorFn ())

    // =========================================================================
    // Extractor Composition
    // =========================================================================

    /// <summary>Composes two extractors, returning a tuple of both results.</summary>
    /// <remarks>Short-circuits on the first error.</remarks>
    let (<&>) (e1: Extractor<'a, 'e>) (e2: Extractor<'b, 'e>) : Extractor<'a * 'b, 'e> =
        fun ctx ->
            match e1 ctx with
            | Error e -> Error e
            | Ok a ->
                match e2 ctx with
                | Error e -> Error e
                | Ok b -> Ok(a, b)

    /// Map over an extractor result
    let internal map (f: 'a -> 'b) (e: Extractor<'a, 'e>) : Extractor<'b, 'e> = fun ctx -> e ctx |> Result.map f

    /// Bind two extractors (flatMap)
    let internal bind (f: 'a -> Extractor<'b, 'e>) (e: Extractor<'a, 'e>) : Extractor<'b, 'e> =
        fun ctx ->
            match e ctx with
            | Error err -> Error err
            | Ok a -> f a ctx

    /// Create an extractor that always succeeds with a value
    let internal succeed (value: 'a) : Extractor<'a, 'e> = fun _ -> Ok value

    /// Create an extractor that always fails with an error
    let internal fail (error: 'e) : Extractor<'a, 'e> = fun _ -> Error error

    // =========================================================================
    // Extractor Execution
    // =========================================================================

    /// <summary>Runs an extractor and converts the result to an HTTP response.</summary>
    /// <param name="toResponse">Converts an error to an HttpHandler (e.g., 401, 400 response).</param>
    /// <param name="extractor">The extractor to run.</param>
    /// <param name="handler">The handler to call with the extracted value on success.</param>
    /// <param name="ctx">The HTTP context (from the returned HttpHandler).</param>
    /// <returns>An HttpHandler that runs extraction and handles the result.</returns>
    /// <example>
    /// <code>
    /// let postHandler (route: PostRoute) : HttpHandler =
    ///     Extraction.run toErrorResponse (hydratePost route) handlePost
    /// </code>
    /// </example>
    let run (toResponse: 'E -> HttpHandler) (extractor: Extractor<'a, 'E>) (handler: 'a -> HttpHandler) : HttpHandler =
        fun (ctx: HttpContext) ->
            task {
                match extractor ctx with
                | Error e -> return! toResponse e ctx
                | Ok a -> return! handler a ctx
            }

    // =========================================================================
    // Internal route helpers (for tests)
    // =========================================================================

    /// Try to parse a GUID from a route parameter
    let internal tryGetRouteGuid (ctx: HttpContext) (paramName: string) : Guid option =
        let route = Request.getRoute ctx
        let idStr = route.GetString paramName

        match Guid.TryParse(idStr) with
        | true, guid -> Some guid
        | false, _ -> None

    /// Require a GUID route parameter, mapping to a typed ID.
    let internal requireRouteId<'TId, 'E>
        (paramName: string)
        (constructor: Guid -> 'TId)
        (error: 'E)
        : Extractor<'TId, 'E> =
        fun ctx -> tryGetRouteGuid ctx paramName |> Option.map constructor |> requireSome error

    /// Require a string route parameter.
    let internal requireRouteStr<'E> (paramName: string) (error: 'E) : Extractor<string, 'E> =
        fun ctx ->
            let route = Request.getRoute ctx
            let value = route.GetString paramName

            if String.IsNullOrEmpty(value) then
                Error error
            else
                Ok value

    /// Require an int route parameter.
    let internal requireRouteInt<'E> (paramName: string) (error: 'E) : Extractor<int, 'E> =
        fun ctx ->
            let route = Request.getRoute ctx
            let value = route.GetString paramName

            match Int32.TryParse(value) with
            | true, i -> Ok i
            | false, _ -> Error error

    /// Require an int route parameter with dynamic error.
    let internal requireRouteIntWith<'E> (paramName: string) (errorFn: unit -> 'E) : Extractor<int, 'E> =
        fun ctx ->
            let route = Request.getRoute ctx
            let value = route.GetString paramName

            match Int32.TryParse(value) with
            | true, i -> Ok i
            | false, _ -> Error(errorFn ())

    /// Ignore extractor result (useful for validation-only checks)
    let internal ignoreResult (e: Extractor<'a, 'e>) : Extractor<unit, 'e> = fun ctx -> e ctx |> Result.map ignore
