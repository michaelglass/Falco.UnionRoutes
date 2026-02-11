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
// Route Constraint Types
// =========================================================================

/// <summary>ASP.NET Core parameterless route constraint kinds.</summary>
/// <remarks>
/// <para>Type-based constraints (e.g., <c>:guid</c>, <c>:int</c>, <c>:bool</c>, <c>:long</c>) are
/// derived implicitly from field types and do not appear in this enum.</para>
/// <para>Use with <see cref="T:Falco.UnionRoutes.RouteAttribute"/> or
/// <see cref="M:Falco.UnionRoutes.Extraction.Extractor.constrainedParser``1"/> to add constraints to route parameters.</para>
/// </remarks>
type RouteConstraint =
    /// <summary>Matches alphabetic characters only (a-z, A-Z).</summary>
    | Alpha = 0
    /// <summary>Matches non-empty values.</summary>
    | Required = 1
    /// <summary>Matches values that look like file names (contain a dot).</summary>
    | File = 2
    /// <summary>Matches values that do not look like file names.</summary>
    | NonFile = 3

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

    /// <summary>Registers a parser for a custom type in route/query parameters.</summary>
    /// <remarks>
    /// <para>Create with <c>Extractor.parser</c>. Only needed for custom types -
    /// built-in types (Guid, string, int, int64, bool) and single-case DU wrappers
    /// are handled automatically.</para>
    /// </remarks>
    /// <example>
    /// <code>
    /// let config: EndpointConfig&lt;AppError&gt; = {
    ///     Parsers = [ Extractor.parser (fun s -> Ok (Slug s)) ]
    ///     // ... other config
    /// }
    /// </code>
    /// </example>
    [<NoComparison; NoEquality>]
    type FieldParser =
        {
            /// The output type this parser produces (e.g., typeof<Slug>)
            ForType: Type
            /// The input type the parser expects (typeof<string> for plain parsers, typeof<bool> for typed, etc.)
            InputType: Type
            /// Parse input value (boxed InputType) into the target type
            Parse: obj -> Result<obj, string>
            /// Explicit route constraints declared by this parser (e.g., [| Alpha |])
            ExplicitConstraints: RouteConstraint[]
        }

    /// <summary>Registers an extractor for PreCondition or OverridablePreCondition fields.</summary>
    /// <typeparam name="E">The error type returned when extraction fails.</typeparam>
    /// <remarks>
    /// <para>Create with <c>Extractor.precondition</c> or <c>Extractor.overridablePrecondition</c>.</para>
    /// <para>Each PreCondition&lt;T&gt; or OverridablePreCondition&lt;T&gt; type used in routes
    /// must have a corresponding extractor registered in EndpointConfig.</para>
    /// </remarks>
    /// <example>
    /// <code>
    /// let config: EndpointConfig&lt;AppError&gt; = {
    ///     Preconditions = [
    ///         Extractor.precondition&lt;UserId, _&gt; (fun ctx ->
    ///             match getAuthCookie ctx with
    ///             | Some id -> Ok id
    ///             | None -> Error NotAuthenticated)
    ///     ]
    ///     // ... other config
    /// }
    /// </code>
    /// </example>
    [<NoComparison; NoEquality>]
    type PreconditionExtractor<'E> =
        {
            /// The marker type this extracts (e.g., typeof<PreCondition<UserId>>)
            ForType: Type
            /// Extract the value from HTTP context
            Extract: HttpContext -> Result<obj, 'E>
        }

    /// <summary>Configuration for Route.endpoints - bundles all extraction settings.</summary>
    /// <typeparam name="E">The error type for extraction failures.</typeparam>
    /// <remarks>
    /// <para>This record combines all the pieces needed for route extraction:</para>
    /// <list type="bullet">
    ///   <item><description>Precondition extractors for auth/validation</description></item>
    ///   <item><description>Custom parsers for domain types</description></item>
    ///   <item><description>Error handling functions</description></item>
    /// </list>
    /// </remarks>
    /// <example>
    /// <code>
    /// let config: EndpointConfig&lt;AppError&gt; = {
    ///     Preconditions = [ Extractor.precondition&lt;UserId, _&gt; requireAuth ]
    ///     Parsers = [ Extractor.parser&lt;Slug&gt; slugParser ]
    ///     MakeError = fun msg -> BadRequest msg
    ///     CombineErrors = fun errors -> errors |> List.head
    ///     ToErrorResponse = fun e -> Response.withStatusCode 400 >> Response.ofPlainText (string e)
    /// }
    ///
    /// let endpoints = Route.endpoints config routeHandler
    /// </code>
    /// </example>
    [<NoComparison; NoEquality>]
    type EndpointConfig<'E> =
        {
            /// Extractors for PreCondition<'T> and OverridablePreCondition<'T> fields.
            /// Create with Extractor.precondition or Extractor.overridablePrecondition.
            Preconditions: PreconditionExtractor<'E> list
            /// Parsers for custom types in route/query parameters.
            /// Create with Extractor.parser. Built-in types are handled automatically.
            Parsers: FieldParser list
            /// Converts a route/query parsing error message (string) into your error type.
            MakeError: string -> 'E
            /// Reduces multiple field errors into one when more than one field fails.
            CombineErrors: 'E list -> 'E
            /// Converts the final error into an HTTP response sent to the client.
            ToErrorResponse: 'E -> HttpHandler
        }

    // =========================================================================
    // Extractor Module - Factory Functions
    // =========================================================================

    /// <summary>Factory functions for creating extractors to use with EndpointConfig.</summary>
    /// <remarks>
    /// <para>Route.endpoints needs to know how to populate each field in your route types:</para>
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
    /// let config: EndpointConfig&lt;AppError&gt; = {
    ///     Preconditions = [ Extractor.precondition&lt;UserId, _&gt; requireAuth ]
    ///     Parsers = [ Extractor.parser&lt;Slug&gt; (fun s -> Ok (Slug s)) ]
    ///     MakeError = fun msg -> BadRequest msg
    ///     CombineErrors = fun errors -> errors |> List.head
    ///     ToErrorResponse = fun e -> Response.ofPlainText (string e)
    /// }
    /// </code>
    /// </example>
    [<RequireQualifiedAccess>]
    module Extractor =
        /// <summary>Registers an extractor for <c>PreCondition&lt;'T&gt;</c> fields.</summary>
        /// <typeparam name="T">The inner type (e.g., UserId in PreCondition&lt;UserId&gt;).</typeparam>
        /// <typeparam name="E">The error type.</typeparam>
        /// <param name="extractor">Function that extracts the value from HTTP context.</param>
        /// <returns>A PreconditionExtractor for EndpointConfig.</returns>
        let precondition<'T, 'E> (extractor: Extractor<'T, 'E>) : PreconditionExtractor<'E> =
            { ForType = typeof<PreCondition<'T>>
              Extract = fun ctx -> extractor ctx |> Result.map (fun v -> box (PreCondition v)) }

        /// <summary>Registers an extractor for <c>OverridablePreCondition&lt;'T&gt;</c> fields.</summary>
        /// <typeparam name="T">The inner type (e.g., AdminId in OverridablePreCondition&lt;AdminId&gt;).</typeparam>
        /// <typeparam name="E">The error type.</typeparam>
        /// <param name="extractor">Function that extracts the value from HTTP context.</param>
        /// <returns>A PreconditionExtractor for EndpointConfig.</returns>
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
        /// <returns>A FieldParser for EndpointConfig.</returns>
        /// <remarks>
        /// <para>Only needed for custom types. Built-in types and single-case DU wrappers
        /// around built-in types are handled automatically.</para>
        /// </remarks>
        let parser<'T> (parser: Parser<'T>) : FieldParser =
            { ForType = typeof<'T>
              InputType = typeof<string>
              Parse = fun o -> parser (o :?> string) |> Result.map box
              ExplicitConstraints = [||] }

        /// <summary>Registers a typed parser where the input type determines the implicit route constraint.</summary>
        /// <typeparam name="TInput">The input type (e.g., bool → :bool constraint, int → :int).</typeparam>
        /// <typeparam name="TOutput">The output type this parser produces.</typeparam>
        /// <param name="parser">Function that converts the pre-parsed typed value into the target type.</param>
        /// <returns>A FieldParser for EndpointConfig.</returns>
        /// <remarks>
        /// <para>The parser receives a pre-parsed typed value (not a raw string) because the ASP.NET Core
        /// route constraint guarantees the format. For example, a <c>typedParser&lt;bool, ToggleState&gt;</c>
        /// receives a <c>bool</c> value since the <c>:bool</c> constraint ensures only valid booleans reach the handler.</para>
        /// </remarks>
        let typedParser<'TInput, 'TOutput> (parser: 'TInput -> Result<'TOutput, string>) : FieldParser =
            { ForType = typeof<'TOutput>
              InputType = typeof<'TInput>
              Parse = fun o -> parser (o :?> 'TInput) |> Result.map box
              ExplicitConstraints = [||] }

        /// <summary>Registers a string parser with explicit route constraints.</summary>
        /// <typeparam name="T">The output type this parser produces.</typeparam>
        /// <param name="constraints">ASP.NET Core route constraints to apply (e.g., Alpha).</param>
        /// <param name="parser">Function that parses a string into the target type.</param>
        /// <returns>A FieldParser for EndpointConfig.</returns>
        /// <remarks>
        /// <para>Use this when you want ASP.NET Core to validate the route parameter before your parser runs.
        /// For example, <c>constrainedParser&lt;Slug&gt; [| RouteConstraint.Alpha |] slugParser</c> adds an
        /// <c>:alpha</c> constraint so only alphabetic values reach the parser.</para>
        /// </remarks>
        let constrainedParser<'T> (constraints: RouteConstraint[]) (parser: Parser<'T>) : FieldParser =
            { ForType = typeof<'T>
              InputType = typeof<string>
              Parse = fun o -> parser (o :?> string) |> Result.map box
              ExplicitConstraints = constraints }

    // =========================================================================
    // Extractor Execution (internal - used by Route.endpoints)
    // =========================================================================

    /// Runs an extractor and converts the result to an HTTP response.
    let internal run
        (toResponse: 'E -> HttpHandler)
        (extractor: Extractor<'a, 'E>)
        (handler: 'a -> HttpHandler)
        : HttpHandler =
        fun (ctx: HttpContext) ->
            task {
                match extractor ctx with
                | Error e -> return! toResponse e ctx
                | Ok a -> return! handler a ctx
            }
