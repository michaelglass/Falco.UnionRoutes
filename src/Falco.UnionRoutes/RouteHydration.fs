namespace Falco.UnionRoutes

open System
open Falco
open Microsoft.AspNetCore.Http
open Microsoft.FSharp.Reflection

/// <summary>A custom type extractor for route hydration.</summary>
/// <remarks>
/// <para>A function that takes (fieldName: string, fieldType: Type, ctx: HttpContext) and returns:</para>
/// <list type="bullet">
///   <item><description><c>Some (Ok value)</c> if extraction succeeds</description></item>
///   <item><description><c>Some (Error message)</c> if extraction fails</description></item>
///   <item><description><c>None</c> to defer to other extractors</description></item>
/// </list>
/// </remarks>
/// <example>
/// <code>
/// let slugExtractor: TypeExtractor =
///     fun fieldName fieldType ctx ->
///         if fieldType = typeof&lt;Slug&gt; then
///             let value = Request.getRoute(ctx).GetString(fieldName)
///             if String.IsNullOrEmpty(value) then Some(Error $"Missing slug: {fieldName}")
///             else Some(Ok(box (Slug value)))
///         else None
/// </code>
/// </example>
type TypeExtractor = string -> Type -> HttpContext -> Result<obj, string> option

/// <summary>Wrapper type to indicate a field should be extracted from query string instead of route params.</summary>
/// <typeparam name="T">The type of the query parameter value.</typeparam>
/// <example>
/// <code>
/// type PostRoute =
///     | Search of query: QueryParam&lt;string&gt;     // extracts from ?query=...
///     | List of page: QueryParam&lt;int&gt; option    // optional query param
/// </code>
/// </example>
type Query<'T> = Query of 'T

/// <summary>Alias for <c>Query&lt;'T&gt;</c> with a more descriptive name.</summary>
/// <typeparam name="T">The type of the query parameter value.</typeparam>
type QueryParam<'T> = Query<'T>

/// <summary>Marker type to indicate a field comes from a precondition (auth, validation, etc.)
/// rather than from route/query parameters.</summary>
/// <typeparam name="T">The type of value provided by the precondition.</typeparam>
/// <remarks>
/// <para><c>Pre&lt;'T&gt;</c> is STRICT - it always runs and cannot be skipped by child routes.</para>
/// <para>Use <see cref="OptPre{T}"/> for skippable preconditions.</para>
/// </remarks>
/// <example>
/// <code>
/// type PostRoute =
///     | List                                    // No preconditions
///     | Create of PreCondition&lt;UserId&gt;          // UserId from auth precondition
///     | Delete of PreCondition&lt;UserId&gt; * id: Guid  // Auth + route param
/// </code>
/// </example>
/// <seealso cref="OptPre{T}"/>
type Pre<'T> = Pre of 'T

/// <summary>Alias for <c>Pre&lt;'T&gt;</c> with a more descriptive name.</summary>
/// <typeparam name="T">The type of value provided by the precondition.</typeparam>
type PreCondition<'T> = Pre<'T>

/// <summary>Marker type for a SKIPPABLE precondition.</summary>
/// <typeparam name="T">The type of value provided by the precondition.</typeparam>
/// <remarks>
/// <para>Child routes can opt out using <see cref="SkipAllPreconditionsAttribute"/> or
/// <see cref="SkipPreconditionAttribute"/>.</para>
/// <para>When skipped, the handler should ignore the value with <c>_</c> pattern.</para>
/// <para>Use <see cref="Pre{T}"/> for strict preconditions that cannot be skipped.</para>
/// </remarks>
/// <example>
/// <code>
/// type UserItemRoute =
///     | List                                    // inherits parent preconditions
///     | [&lt;SkipAllPreconditions&gt;] Public         // skips OptPre, handler uses _
///
/// type Route =
///     | Users of Pre&lt;AdminId&gt; * OptPre&lt;UserId&gt; * UserRoute
///
/// // Handler:
/// match route with
/// | Users (Pre adminId, _, Items Public) ->
///     handlePublic adminId  // userId skipped, use _ pattern
/// </code>
/// </example>
/// <seealso cref="Pre{T}"/>
type OptPre<'T> = OptPre of 'T

/// <summary>Alias for <c>OptPre&lt;'T&gt;</c> with a more descriptive name.</summary>
/// <typeparam name="T">The type of value provided by the precondition.</typeparam>
type OptionalPreCondition<'T> = OptPre<'T>

/// <summary>A precondition that provides a value of a specific type from the HTTP context.</summary>
/// <typeparam name="Error">The error type returned when the precondition fails.</typeparam>
/// <remarks>
/// Used for auth, validation, or any computation that should run before route handling.
/// Create instances using <see cref="RouteHydration.forPre"/> or <see cref="RouteHydration.forOptPre"/>.
/// </remarks>
type Precondition<'Error> =
    { /// <summary>The type this precondition matches (e.g., <c>typeof&lt;Pre&lt;UserId&gt;&gt;</c>).</summary>
      MatchType: Type
      /// <summary>The extraction function that runs the precondition.</summary>
      Extract: HttpContext -> Result<obj, 'Error> }

/// <summary>Automatic route hydration based on route type structure.</summary>
/// <remarks>
/// <para>Examines each field in a route case and extracts values based on type:</para>
/// <list type="bullet">
///   <item><description><c>Pre&lt;'T&gt;</c> - looks up a precondition for that type</description></item>
///   <item><description><c>OptPre&lt;'T&gt;</c> - skippable precondition</description></item>
///   <item><description>Custom extractors are tried next (if provided)</description></item>
///   <item><description>Primitives (Guid, string, int, int64, bool) - extract from route params</description></item>
///   <item><description><c>Query&lt;'T&gt;</c> - extracts from query string (required)</description></item>
///   <item><description><c>Query&lt;'T&gt; option</c> - extracts from query string (optional)</description></item>
/// </list>
/// <para>Wrapper types (single-case DUs like <c>type PostId = PostId of Guid</c>) are
/// automatically detected and unwrapped for extraction, then re-wrapped.</para>
/// </remarks>
/// <example>
/// <code>
/// type PostId = PostId of Guid
/// type PostRoute =
///     | List of page: Query&lt;int&gt; option   // optional query param
///     | Detail of PostId                  // extracts Guid, wraps in PostId
///     | Create of Pre&lt;UserId&gt;             // precondition field
///     | Delete of Pre&lt;UserId&gt; * PostId
///     | Search of query: Query&lt;string&gt;    // required query param
///
/// let preconditions = [ forPre&lt;UserId, AppError&gt; requireAuth ]
/// let hydrate = RouteHydration.create&lt;PostRoute, AppError&gt; preconditions [] makeError combineErrors
/// </code>
/// </example>
[<RequireQualifiedAccess>]
module RouteHydration =

    /// <summary>Creates a strict precondition for <c>Pre&lt;'T&gt;</c> that runs the given pipeline.</summary>
    /// <typeparam name="T">The type of value the precondition provides.</typeparam>
    /// <typeparam name="Error">The error type returned on failure.</typeparam>
    /// <param name="pipeline">The pipeline that extracts the value from HTTP context.</param>
    /// <returns>A precondition that can be passed to <see cref="create"/>.</returns>
    /// <remarks>The result is automatically wrapped in <c>Pre</c>. This precondition cannot be skipped.</remarks>
    /// <example>
    /// <code>
    /// let authPrecondition = RouteHydration.forPre&lt;UserId, AppError&gt; requireAuth
    /// </code>
    /// </example>
    let forPre<'T, 'Error> (pipeline: Pipeline<'T, 'Error>) : Precondition<'Error> =
        { MatchType = typeof<Pre<'T>>
          Extract = fun ctx -> pipeline ctx |> Result.map (fun v -> box (Pre v)) }

    /// <summary>Creates a skippable precondition for <c>OptPre&lt;'T&gt;</c> that runs the given pipeline.</summary>
    /// <typeparam name="T">The type of value the precondition provides.</typeparam>
    /// <typeparam name="Error">The error type returned on failure.</typeparam>
    /// <param name="pipeline">The pipeline that extracts the value from HTTP context.</param>
    /// <returns>A precondition that can be passed to <see cref="create"/>.</returns>
    /// <remarks>
    /// The result is automatically wrapped in <c>OptPre</c>.
    /// Child routes can skip this using <see cref="SkipAllPreconditionsAttribute"/> or <see cref="SkipPreconditionAttribute"/>.
    /// </remarks>
    /// <example>
    /// <code>
    /// let optAdminPrecondition = RouteHydration.forOptPre&lt;AdminId, AppError&gt; requireAdmin
    /// </code>
    /// </example>
    let forOptPre<'T, 'Error> (pipeline: Pipeline<'T, 'Error>) : Precondition<'Error> =
        { MatchType = typeof<OptPre<'T>>
          Extract = fun ctx -> pipeline ctx |> Result.map (fun v -> box (OptPre v)) }

    /// Detects if a type is Pre<'T>.
    /// Returns Some innerType if so, None otherwise.
    let private tryGetPreInfo (t: Type) : Type option =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Pre<_>> then
            Some(t.GetGenericArguments().[0])
        else
            None

    /// Detects if a type is OptPre<'T>.
    /// Returns Some innerType if so, None otherwise.
    let private tryGetOptPreInfo (t: Type) : Type option =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<OptPre<_>> then
            Some(t.GetGenericArguments().[0])
        else
            None

    /// Detects if a type is a single-case DU wrapping another type.
    /// Returns Some (innerType, constructor) if so, None otherwise.
    let private tryGetWrapperInfo (t: Type) : (Type * (obj -> obj)) option =
        if FSharpType.IsUnion t then
            let cases = FSharpType.GetUnionCases t

            if cases.Length = 1 then
                let case = cases.[0]
                let fields = case.GetFields()

                if fields.Length = 1 then
                    let innerType = fields.[0].PropertyType
                    let constructor = fun (value: obj) -> FSharpValue.MakeUnion(case, [| value |])
                    Some(innerType, constructor)
                else
                    None
            else
                None
        else
            None

    /// Checks if a type is a tuple.
    let private isTupleType (t: Type) = FSharpType.IsTuple t

    /// Checks if a type is an F# record.
    let private isRecordType (t: Type) = FSharpType.IsRecord t

    /// Checks if a type is an anonymous record.
    let private isAnonymousRecordType (t: Type) =
        t.Name.StartsWith("<>f__AnonymousType")
        || (t.GetCustomAttributes(typeof<CompilationMappingAttribute>, false)
            |> Array.exists (fun attr ->
                (attr :?> CompilationMappingAttribute).SourceConstructFlags = SourceConstructFlags.RecordType)
            && t.Name.Contains("@"))

    /// Detects if a type is an option type.
    /// Returns Some innerType if so, None otherwise.
    let private tryGetOptionInfo (t: Type) : Type option =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
            Some(t.GetGenericArguments().[0])
        else
            None

    /// Detects if a type is Query<'T>.
    /// Returns Some innerType if so, None otherwise.
    let private tryGetQueryInfo (t: Type) : Type option =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Query<_>> then
            Some(t.GetGenericArguments().[0])
        else
            None

    // =========================================================================
    // Nested route union detection
    // =========================================================================

    /// Check if a type is a nested route union (for hierarchy traversal)
    /// Excludes: strings, options, Pre<'T>, OptPre<'T>, Query<'T>, single-case wrappers
    let private isNestedRouteUnion (t: Type) =
        FSharpType.IsUnion(t)
        && t <> typeof<string>
        && not (t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>)
        && not (tryGetPreInfo t |> Option.isSome)
        && not (tryGetOptPreInfo t |> Option.isSome)
        && not (tryGetQueryInfo t |> Option.isSome)
        && not (tryGetWrapperInfo t |> Option.isSome)

    // =========================================================================
    // Skip attribute detection
    // =========================================================================

    /// Check if all OptPre preconditions should be skipped for a case
    let private shouldSkipAllOptPreconditions (caseInfo: UnionCaseInfo) : bool =
        caseInfo.GetCustomAttributes(typeof<SkipAllPreconditionsAttribute>)
        |> Array.isEmpty
        |> not

    /// Get list of OptPre inner types to skip for a case
    let private getSkippedPreconditionTypes (caseInfo: UnionCaseInfo) : Type list =
        caseInfo.GetCustomAttributes(typeof<SkipPreconditionAttribute>)
        |> Array.map (fun a -> (a :?> SkipPreconditionAttribute).PreconditionType)
        |> Array.toList

    /// Check if a specific OptPre type should be skipped
    let private shouldSkipOptPrecondition (caseInfo: UnionCaseInfo) (innerType: Type) : bool =
        shouldSkipAllOptPreconditions caseInfo
        || getSkippedPreconditionTypes caseInfo |> List.contains innerType

    /// Walk route hierarchy to find the leaf case info
    /// Returns the deepest union case in the route hierarchy
    let rec private findLeafCaseInfo (value: obj) : UnionCaseInfo =
        let valueType = value.GetType()

        if not (FSharpType.IsUnion(valueType)) then
            failwith $"Expected union type, got {valueType.Name}"
        else
            let caseInfo, fieldValues = FSharpValue.GetUnionFields(value, valueType)
            let fields = caseInfo.GetFields()

            // Check if any field is a nested route union
            let nestedUnionFieldIndex =
                fields
                |> Array.tryFindIndex (fun f ->
                    FSharpType.IsUnion(f.PropertyType)
                    && f.PropertyType <> typeof<string>
                    && not (
                        f.PropertyType.IsGenericType
                        && f.PropertyType.GetGenericTypeDefinition() = typedefof<option<_>>
                    )
                    && not (tryGetPreInfo f.PropertyType |> Option.isSome)
                    && not (tryGetOptPreInfo f.PropertyType |> Option.isSome)
                    && not (tryGetQueryInfo f.PropertyType |> Option.isSome)
                    && not (tryGetWrapperInfo f.PropertyType |> Option.isSome))

            match nestedUnionFieldIndex with
            | Some idx ->
                let nestedValue = fieldValues.[idx]
                findLeafCaseInfo nestedValue
            | None ->
                // This is the leaf case
                caseInfo

    /// Creates a Some value boxed as option<'T>
    let private boxSome (innerType: Type) (value: obj) : obj =
        let someCase =
            FSharpType.GetUnionCases(typedefof<option<_>>.MakeGenericType(innerType))
            |> Array.find (fun c -> c.Name = "Some")

        FSharpValue.MakeUnion(someCase, [| value |])

    /// Creates a None value boxed as option<'T>
    let private boxNone (innerType: Type) : obj =
        let noneCase =
            FSharpType.GetUnionCases(typedefof<option<_>>.MakeGenericType(innerType))
            |> Array.find (fun c -> c.Name = "None")

        FSharpValue.MakeUnion(noneCase, [||])

    /// Source of parameter values (route or query string)
    type private ParamSource =
        | Route
        | QueryString

    /// Supported primitive types for route/query extraction
    let private supportedPrimitives =
        [ typeof<Guid>; typeof<string>; typeof<int>; typeof<int64>; typeof<bool> ]

    /// Check if a type is a supported primitive for extraction
    let private isSupportedPrimitive (t: Type) = supportedPrimitives |> List.contains t

    /// Tries to parse a string value into a primitive type.
    /// Returns Ok value, Error for invalid format, or None if type not supported.
    let private tryParsePrimitive (primitiveType: Type) (value: string) (fieldName: string) (source: ParamSource) =
        let sourceName =
            match source with
            | Route -> "route"
            | QueryString -> "query"

        if primitiveType = typeof<Guid> then
            match Guid.TryParse(value) with
            | true, guid -> Some(Ok(box guid))
            | false, _ -> Some(Error $"Invalid GUID {sourceName} parameter: {fieldName}")
        elif primitiveType = typeof<string> then
            Some(Ok(box value))
        elif primitiveType = typeof<int> then
            match Int32.TryParse(value) with
            | true, i -> Some(Ok(box i))
            | false, _ -> Some(Error $"Invalid integer {sourceName} parameter: {fieldName}")
        elif primitiveType = typeof<int64> then
            match Int64.TryParse(value) with
            | true, i -> Some(Ok(box i))
            | false, _ -> Some(Error $"Invalid int64 {sourceName} parameter: {fieldName}")
        elif primitiveType = typeof<bool> then
            match Boolean.TryParse(value) with
            | true, b -> Some(Ok(box b))
            | false, _ -> Some(Error $"Invalid boolean {sourceName} parameter: {fieldName}")
        else
            None

    /// Gets a string value from route parameters.
    /// Reads directly from RouteValues to avoid Falco's GetString which converts large numbers to scientific notation.
    let private getRouteString (ctx: HttpContext) (fieldName: string) : string =
        match ctx.Request.RouteValues.TryGetValue(fieldName) with
        | true, value when not (isNull value) -> value.ToString()
        | true, _nullValue -> ""
        | false, _ -> ""

    /// Extracts a primitive value from a parameter source.
    /// Returns Ok value, Error for invalid, or None if missing (for optional handling).
    let private tryExtractPrimitive
        (source: ParamSource)
        (fieldName: string)
        (primitiveType: Type)
        (ctx: HttpContext)
        : Result<obj, string> option =

        let value =
            match source with
            | Route -> getRouteString ctx fieldName
            | QueryString -> (Request.getQuery ctx).GetString fieldName

        if String.IsNullOrEmpty(value) then
            None // Missing
        else
            match tryParsePrimitive primitiveType value fieldName source with
            | Some result -> Some result
            | None ->
                let sourceName =
                    match source with
                    | Route -> "route"
                    | QueryString -> "query"

                Some(Error $"Unsupported primitive type for {sourceName}: {primitiveType.Name}")

    /// Tries custom extractors in order, returning the first successful extraction.
    let private tryCustomExtractors
        (extractors: TypeExtractor list)
        (fieldName: string)
        (fieldType: Type)
        (ctx: HttpContext)
        : Result<obj, string> option =
        extractors |> List.tryPick (fun extractor -> extractor fieldName fieldType ctx)

    /// Extracts a required value from route or query.
    let private extractRequired
        (extractors: TypeExtractor list)
        (fieldName: string)
        (fieldType: Type)
        (ctx: HttpContext)
        : Result<obj, string> =

        // Try custom extractors first
        match tryCustomExtractors extractors fieldName fieldType ctx with
        | Some result -> result
        | None ->
            // Check for Query<'T> wrapper
            match tryGetQueryInfo fieldType with
            | Some innerType ->
                // Extract from query string, wrap in Query<'T>
                match tryExtractPrimitive QueryString fieldName innerType ctx with
                | Some(Ok v) ->
                    let queryCase = FSharpType.GetUnionCases(fieldType).[0]
                    Ok(FSharpValue.MakeUnion(queryCase, [| v |]))
                | Some(Error msg) -> Error msg
                | None -> Error $"Missing query parameter: {fieldName}"
            | None ->
                // Check for primitives (Guid, string, int, int64, bool)
                if isSupportedPrimitive fieldType then
                    tryExtractPrimitive Route fieldName fieldType ctx
                    |> Option.defaultValue (Error $"Missing route parameter: {fieldName}")
                else
                    // Check if it's a wrapper type (single-case DU like PostId)
                    match tryGetWrapperInfo fieldType with
                    | Some(innerType, wrapFn) ->
                        tryExtractPrimitive Route fieldName innerType ctx
                        |> Option.defaultValue (Error $"Missing route parameter: {fieldName}")
                        |> Result.map wrapFn
                    | None ->
                        // Provide helpful error messages for unsupported types
                        if isTupleType fieldType then
                            failwith
                                $"RouteHydration: Tuple types are not supported for field '{fieldName}'. Use named fields instead: '| Case of a: Guid * b: int'"
                        elif isRecordType fieldType then
                            failwith
                                $"RouteHydration: Record types are not yet supported for field '{fieldName}'. Use named fields directly on the union case: '| Case of id: Guid * name: string'"
                        elif isAnonymousRecordType fieldType then
                            failwith
                                $"RouteHydration: Anonymous record types are not supported for field '{fieldName}'. Use named fields directly on the union case."
                        else
                            failwith
                                $"RouteHydration: Don't know how to extract field '{fieldName}' of type {fieldType.Name}. Supported: Guid, string, int, int64, bool, Query<T>, PreCondition<T>, or single-case wrapper DUs."

    /// Extracts a field value from route parameters based on type.
    /// Handles option types, Query types, custom extractors, primitives, and wrapper types.
    let private extractField
        (extractors: TypeExtractor list)
        (fieldName: string)
        (fieldType: Type)
        (ctx: HttpContext)
        : Result<obj, string> =

        // Check for option type - only supported for Query<'T> option
        match tryGetOptionInfo fieldType with
        | Some innerType ->
            match tryGetQueryInfo innerType with
            | Some queryInnerType ->
                // Query<'T> option - extract from query, return None if missing
                match tryExtractPrimitive QueryString fieldName queryInnerType ctx with
                | Some(Ok v) ->
                    let queryCase = FSharpType.GetUnionCases(innerType).[0]
                    let queryVal = FSharpValue.MakeUnion(queryCase, [| v |])
                    Ok(boxSome innerType queryVal)
                | Some(Error msg) -> Error msg
                | None -> Ok(boxNone innerType)
            | None ->
                failwith
                    $"RouteHydration: Option types only supported for Query<'T>. Use Query<{innerType.Name}> option for optional query params."
        | None ->
            // Not an option type - required field
            extractRequired extractors fieldName fieldType ctx

    /// Finds a precondition that matches the given type.
    let private findPrecondition (preconditions: Precondition<'Error> list) (fieldType: Type) =
        preconditions |> List.tryFind (fun p -> p.MatchType = fieldType)

    /// Creates a default value for a type, handling wrapper types recursively.
    let rec private createDefaultValue (t: Type) : obj =
        if t.IsValueType then
            Activator.CreateInstance(t)
        elif t = typeof<string> then
            box ""
        else
            // Check if it's a single-case DU wrapper
            match tryGetWrapperInfo t with
            | Some(innerType, wrapFn) ->
                // Recursively create default for inner type, then wrap
                let innerDefault = createDefaultValue innerType
                wrapFn innerDefault
            | None ->
                // For other reference types, use null
                null

    /// Creates a sentinel (default) value for an OptPre<'T> when skipped.
    /// Returns OptPre(default<'T>) boxed as obj.
    let private createSkippedOptPreValue (optPreType: Type) : obj =
        let innerType = optPreType.GetGenericArguments().[0]
        let defaultValue = createDefaultValue innerType
        let optPreCase = FSharpType.GetUnionCases(optPreType).[0]
        FSharpValue.MakeUnion(optPreCase, [| defaultValue |])

    /// Result of extracting a single field value.
    /// Each field is either from a precondition or from route/query extraction.
    type private FieldResult<'Error> =
        | FromPrecondition of Result<obj, 'Error>
        | FromExtraction of Result<obj, string>

    // =========================================================================
    // Precondition validation
    // =========================================================================

    /// Recursively collects all Pre<T> and OptPre<T> types from a union type
    let rec private collectPreconditionTypes (unionType: Type) : Type list =
        if not (FSharpType.IsUnion(unionType)) then
            []
        else
            FSharpType.GetUnionCases(unionType)
            |> Array.toList
            |> List.collect (fun case ->
                let fields = case.GetFields()

                // Collect Pre<T> and OptPre<T> from this case's fields
                let preconditionTypes =
                    fields
                    |> Array.choose (fun f ->
                        if tryGetPreInfo f.PropertyType |> Option.isSome then
                            Some f.PropertyType
                        elif tryGetOptPreInfo f.PropertyType |> Option.isSome then
                            Some f.PropertyType
                        else
                            None)
                    |> Array.toList

                // Recursively collect from nested route unions
                let nestedTypes =
                    fields
                    |> Array.filter (fun f -> isNestedRouteUnion f.PropertyType)
                    |> Array.toList
                    |> List.collect (fun f -> collectPreconditionTypes f.PropertyType)

                preconditionTypes @ nestedTypes)
            |> List.distinct

    /// Formats a type name in a readable way (e.g., "Pre<UserId>" instead of "Pre`1")
    let private formatTypeName (t: Type) : string =
        if t.IsGenericType then
            let baseName = t.Name.Substring(0, t.Name.IndexOf('`'))

            let args =
                t.GetGenericArguments() |> Array.map (fun a -> a.Name) |> String.concat ", "

            $"{baseName}<{args}>"
        else
            t.Name

    /// <summary>Validates that all <c>Pre&lt;T&gt;</c> and <c>OptPre&lt;T&gt;</c> fields have registered preconditions.</summary>
    /// <typeparam name="Route">The route union type to validate.</typeparam>
    /// <typeparam name="Error">The error type used by preconditions.</typeparam>
    /// <param name="preconditions">The list of registered preconditions.</param>
    /// <returns><c>Ok ()</c> if all precondition types are covered, <c>Error</c> with list of missing types if invalid.</returns>
    /// <remarks>Call this at application startup to catch missing precondition registrations early.</remarks>
    /// <example>
    /// <code>
    /// let allPreconditions = [ authPrecondition(); adminPrecondition() ]
    /// match RouteHydration.validatePreconditions&lt;Route, AppError&gt; allPreconditions with
    /// | Ok () -> ()
    /// | Error errors -> failwith (String.concat "\n" errors)
    /// </code>
    /// </example>
    let validatePreconditions<'Route, 'Error> (preconditions: Precondition<'Error> list) : Result<unit, string list> =
        let requiredTypes = collectPreconditionTypes typeof<'Route>
        let registeredTypes = preconditions |> List.map (fun p -> p.MatchType)

        let missingTypes =
            requiredTypes
            |> List.filter (fun t -> not (registeredTypes |> List.exists (fun r -> r = t)))
            |> List.map formatTypeName

        if missingTypes.IsEmpty then
            Ok()
        else
            let missingStr = missingTypes |> String.concat ", "
            Error [ $"Missing preconditions for: {missingStr}" ]

    /// <summary>Full validation: route structure + precondition coverage.</summary>
    /// <typeparam name="Route">The route union type to validate.</typeparam>
    /// <typeparam name="Error">The error type used by preconditions.</typeparam>
    /// <param name="preconditions">The list of registered preconditions.</param>
    /// <returns><c>Ok ()</c> if valid, <c>Error</c> with list of all issues if invalid.</returns>
    /// <remarks>
    /// Designed for use in tests. Combines <see cref="RouteReflection.validateStructure"/>
    /// and <see cref="validatePreconditions"/> into a single validation call.
    /// </remarks>
    /// <example>
    /// <code>
    /// [&lt;Fact&gt;]
    /// let ``all routes are valid`` () =
    ///     let preconditions = [ authPrecondition(); adminPrecondition() ]
    ///     let result = RouteHydration.validate&lt;Route, AppError&gt; preconditions
    ///     Assert.Equal(Ok (), result)
    /// </code>
    /// </example>
    let validate<'Route, 'Error> (preconditions: Precondition<'Error> list) : Result<unit, string list> =
        let structureErrors =
            match RouteReflection.validateStructure<'Route> () with
            | Ok() -> []
            | Error errors -> errors

        let preconditionErrors =
            match validatePreconditions<'Route, 'Error> preconditions with
            | Ok() -> []
            | Error errors -> errors

        let allErrors = structureErrors @ preconditionErrors

        if allErrors.IsEmpty then Ok() else Error allErrors

    // =========================================================================
    // Hydration
    // =========================================================================

    /// <summary>Creates a hydration function for a route type.</summary>
    /// <typeparam name="Route">The route union type to hydrate.</typeparam>
    /// <typeparam name="Error">The error type for the application.</typeparam>
    /// <param name="preconditions">List of preconditions created with <see cref="forPre"/> or <see cref="forOptPre"/>.</param>
    /// <param name="extractors">Custom type extractors for domain-specific types.</param>
    /// <param name="makeError">Function to convert extraction error strings to the error type.</param>
    /// <param name="combineErrors">Function to combine multiple errors into one.</param>
    /// <returns>A function that takes a route and returns a pipeline that hydrates it.</returns>
    /// <remarks>
    /// <para>The hydration function examines each field and extracts values based on type:</para>
    /// <list type="bullet">
    ///   <item><description><c>Pre&lt;'T&gt;</c> - uses matching precondition (ALWAYS runs, errors preserve type)</description></item>
    ///   <item><description><c>OptPre&lt;'T&gt;</c> - uses matching precondition, can be skipped by child routes</description></item>
    ///   <item><description>Custom extractors are tried next (in order)</description></item>
    ///   <item><description>Primitives (Guid, string, int, int64, bool) - extract from route params</description></item>
    ///   <item><description>Single-case DU wrappers (like <c>PostId of Guid</c>) - auto-detected</description></item>
    ///   <item><description><c>Query&lt;'T&gt;</c> - extracts from query string (required)</description></item>
    ///   <item><description><c>Query&lt;'T&gt; option</c> - extracts from query string (optional)</description></item>
    /// </list>
    /// <para>All errors are accumulated and combined using <paramref name="combineErrors"/>.</para>
    /// </remarks>
    /// <example>
    /// <code>
    /// let hydratePost: PostRoute -> Pipeline&lt;PostRoute, AppError&gt; =
    ///     RouteHydration.create&lt;PostRoute, AppError&gt;
    ///         [ authPrecondition() ]
    ///         [ slugExtractor ]
    ///         makeError
    ///         combineErrors
    /// </code>
    /// </example>
    let create<'Route, 'Error>
        (preconditions: Precondition<'Error> list)
        (extractors: TypeExtractor list)
        (makeError: string -> 'Error)
        (combineErrors: 'Error list -> 'Error)
        : 'Route -> Pipeline<'Route, 'Error> =

        let routeType = typeof<'Route>

        fun (route: 'Route) ->
            fun (ctx: HttpContext) ->
                // Get the union case and current field values
                let caseInfo, fieldValues = FSharpValue.GetUnionFields(route, routeType)
                let fields = caseInfo.GetFields()

                if fields.Length = 0 then
                    // No fields to extract - just return the route as-is
                    Ok route
                else
                    // Find the leaf case to check for skip attributes
                    let leafCaseInfo = findLeafCaseInfo (box route)

                    // Extract each field - either from precondition, route/query params, or pass-through for nested unions
                    let fieldResults =
                        fields
                        |> Array.mapi (fun i field ->
                            // Check if this is a nested route union - pass through without extraction
                            if isNestedRouteUnion field.PropertyType then
                                FromExtraction(Ok(fieldValues.[i]))
                            // Check if this is an OptPre<'T> that should be skipped
                            elif
                                tryGetOptPreInfo field.PropertyType
                                |> Option.exists (fun innerType -> shouldSkipOptPrecondition leafCaseInfo innerType)
                            then
                                // Skip this OptPre - provide sentinel value
                                FromExtraction(Ok(createSkippedOptPreValue field.PropertyType))
                            else
                                // Not skipped - try precondition or extraction
                                match findPrecondition preconditions field.PropertyType with
                                | Some precondition -> FromPrecondition(precondition.Extract ctx)
                                | None -> FromExtraction(extractField extractors field.Name field.PropertyType ctx))

                    // Collect all errors
                    let allErrors =
                        fieldResults
                        |> Array.choose (function
                            | FromPrecondition(Error e) -> Some e
                            | FromExtraction(Error e) -> Some(makeError e)
                            | FromPrecondition(Ok _) -> None
                            | FromExtraction(Ok _) -> None)
                        |> Array.toList

                    if allErrors.Length > 0 then
                        Error(combineErrors allErrors)
                    else
                        // All succeeded - construct the hydrated route
                        let values =
                            fieldResults
                            |> Array.map (function
                                | FromPrecondition(Ok v) -> v
                                | FromExtraction(Ok v) -> v
                                // Error cases already handled above - these are unreachable
                                | FromPrecondition(Error _) -> failwith "unreachable"
                                | FromExtraction(Error _) -> failwith "unreachable")

                        let hydrated = FSharpValue.MakeUnion(caseInfo, values) :?> 'Route
                        Ok hydrated
