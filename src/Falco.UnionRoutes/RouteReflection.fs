namespace Falco.UnionRoutes

open System
open System.Text.RegularExpressions
open Falco
open Falco.Routing
open FSharp.Reflection

/// <summary>Reflection utilities for extracting route information from discriminated unions.</summary>
/// <remarks>
/// Provides functions to generate Falco endpoints, create type-safe links,
/// enumerate all routes, and validate route structure.
/// </remarks>
module RouteReflection =

    // Precompiled regex for better performance during route enumeration
    let private kebabCaseRegex =
        Regex(@"([a-z])([A-Z])|([A-Z]+)([A-Z][a-z])", RegexOptions.Compiled)

    /// <summary>Converts PascalCase to kebab-case.</summary>
    /// <param name="s">The PascalCase string to convert.</param>
    /// <returns>The kebab-case version of the string.</returns>
    /// <example>
    /// <code>
    /// toKebabCase "DigestView"  // returns "digest-view"
    /// toKebabCase "HTMLParser"  // returns "html-parser"
    /// </code>
    /// </example>
    let toKebabCase (s: string) =
        kebabCaseRegex
            .Replace(
                s,
                fun m ->
                    if m.Groups.[1].Success then
                        $"{m.Groups.[1].Value}-{m.Groups.[2].Value}"
                    else
                        $"{m.Groups.[3].Value}-{m.Groups.[4].Value}"
            )
            .ToLowerInvariant()

    /// <summary>Converts a <see cref="RouteMethod"/> enum to an <see cref="HttpMethod"/> discriminated union.</summary>
    /// <param name="rm">The RouteMethod enum value.</param>
    /// <returns>The corresponding HttpMethod DU case.</returns>
    /// <exception cref="System.Exception">Thrown when an unknown RouteMethod value is provided.</exception>
    let toHttpMethod (rm: RouteMethod) : HttpMethod =
        match rm with
        | RouteMethod.Get -> HttpMethod.Get
        | RouteMethod.Post -> HttpMethod.Post
        | RouteMethod.Put -> HttpMethod.Put
        | RouteMethod.Delete -> HttpMethod.Delete
        | RouteMethod.Patch -> HttpMethod.Patch
        | RouteMethod.Any -> HttpMethod.Any
        | unknown -> failwith $"Unknown RouteMethod: {int unknown}"

    /// <summary>Gets the <see cref="RouteAttribute"/> from a union case, if present.</summary>
    /// <param name="case">The union case to inspect.</param>
    /// <returns><c>Some attribute</c> if the case has a RouteAttribute, <c>None</c> otherwise.</returns>
    let getRouteAttr (case: UnionCaseInfo) : RouteAttribute option =
        case.GetCustomAttributes(typeof<RouteAttribute>)
        |> Array.tryHead
        |> Option.map (fun a -> a :?> RouteAttribute)

    /// Get the Path from a RouteAttribute as Option (handles CLR null)
    let private getAttrPath (attr: RouteAttribute) : string option =
        match attr.Path with
        | null -> None
        | path -> Some path

    /// Check if a type is Pre<'T> (precondition marker - should not be in route path)
    /// Detected by name since Pre<'T> is defined in RouteHydration.fs (compiled later)
    let private isPreconditionType (t: Type) =
        t.IsGenericType
        && t.GetGenericTypeDefinition().FullName = "Falco.UnionRoutes.Pre`1"

    /// Check if a type is OptPre<'T> (skippable precondition marker - should not be in route path)
    /// Detected by name since OptPre<'T> is defined in RouteHydration.fs (compiled later)
    let private isOptionalPreconditionType (t: Type) =
        t.IsGenericType
        && t.GetGenericTypeDefinition().FullName = "Falco.UnionRoutes.OptPre`1"

    /// Check if a type is Query<'T> (query parameter - should not be in route path)
    /// Detected by name since Query<'T> is defined in RouteHydration.fs (compiled later)
    let private isQueryType (t: Type) =
        t.IsGenericType
        && t.GetGenericTypeDefinition().FullName = "Falco.UnionRoutes.Query`1"

    /// Check if a type is Query<'T> option (optional query parameter - should not be in route path)
    let private isOptionalQueryType (t: Type) =
        t.IsGenericType
        && t.GetGenericTypeDefinition() = typedefof<option<_>>
        && t.GetGenericArguments().[0].IsGenericType
        && t.GetGenericArguments().[0].GetGenericTypeDefinition().FullName = "Falco.UnionRoutes.Query`1"

    /// Supported primitive types for route/query extraction
    let private supportedPrimitives =
        [ typeof<Guid>; typeof<string>; typeof<int>; typeof<int64>; typeof<bool> ]

    /// Check if a type is a single-case DU wrapper for a primitive (e.g., PostId of Guid)
    /// These are used for type-safe IDs, not for route hierarchy.
    /// Only matches wrappers around primitive types - not wrappers around Pre<'T> etc.
    let private isSingleCaseWrapper (t: Type) =
        FSharpType.IsUnion(t)
        && let cases = FSharpType.GetUnionCases(t) in

           cases.Length = 1
           && cases.[0].GetFields().Length = 1
           && supportedPrimitives |> List.contains (cases.[0].GetFields().[0].PropertyType)

    /// Check if a type is a nested route union (for hierarchy traversal)
    /// Excludes: strings, options, Pre<'T>, OptPre<'T>, Query<'T>, OptQuery<'T>, single-case wrappers
    let private isNestedRouteUnion (t: Type) =
        FSharpType.IsUnion(t)
        && t <> typeof<string>
        && not (t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>)
        && not (isPreconditionType t)
        && not (isOptionalPreconditionType t)
        && not (isQueryType t)
        && not (isOptionalQueryType t)
        && not (isSingleCaseWrapper t)

    /// Check if a field should be excluded from route path (nested route unions, preconditions, query params)
    let private isNonRouteField (f: Reflection.PropertyInfo) =
        isNestedRouteUnion f.PropertyType
        || isPreconditionType f.PropertyType
        || isOptionalPreconditionType f.PropertyType
        || isQueryType f.PropertyType
        || isOptionalQueryType f.PropertyType

    /// Infer path segment from case fields (e.g., "of id: Guid" -> "{id}")
    /// Excludes: nested route unions, Pre<'T> (preconditions), OptPre<'T> (skippable preconditions), Query<'T> (query params)
    /// RESTful conventions (Show, List, Delete, Patch, Edit, Create): just param path (no case name prefix)
    /// Other cases with nested union: include case name as prefix (e.g., "emails/{id}")
    /// Cases without nested union: just param path
    let private inferPathFromFields (case: UnionCaseInfo) : string option =
        let fields = case.GetFields()

        if fields.Length = 0 then
            None
        elif fields.Length = 1 && isNestedRouteUnion fields.[0].PropertyType then
            None // Nested route union only, not a path parameter
        else
            // Filter out non-route fields (nested unions, preconditions, query params)
            let pathFields = fields |> Array.filter (fun f -> not (isNonRouteField f))

            let hasNestedUnion =
                fields |> Array.exists (fun f -> isNestedRouteUnion f.PropertyType)

            if pathFields.Length = 0 then
                None
            else
                let paramPath =
                    pathFields |> Array.map (fun f -> "{" + f.Name + "}") |> String.concat "/"

                // RESTful conventions: just the param path (e.g., "{id}" for /users/{id})
                // Other cases with nested union: include case name prefix (e.g., "emails/{id}")
                match case.Name, hasNestedUnion with
                | "Show", _
                | "List", _
                | "Delete", _
                | "Patch", _
                | "Edit", _
                | "Create", _ -> Some paramPath
                | _, true -> Some(toKebabCase case.Name + "/" + paramPath)
                | _, false -> Some paramPath

    /// Check if a case has any route path fields (i.e., typed arguments like "of id: Guid")
    /// Excludes: nested route unions, Pre<'T> (preconditions), Query<'T> (query params)
    let private hasRoutePathFields (case: UnionCaseInfo) : bool =
        case.GetFields() |> Array.exists (fun f -> not (isNonRouteField f))

    /// Get path segment from a case (uses attribute Path if set, otherwise infers from fields or case name)
    /// RESTful case names (Root, List, Create, Show, Delete, Patch, Edit) return empty path when no params
    let private getPathSegment (case: UnionCaseInfo) : string =
        match getRouteAttr case |> Option.bind getAttrPath with
        | Some path -> path
        | None ->
            // First try to infer from fields
            match inferPathFromFields case with
            | Some path -> path
            | None ->
                // No inferable fields, check for special RESTful case names
                match case.Name with
                | "Root"
                | "List"
                | "Create"
                | "Show"
                | "Delete"
                | "Patch"
                | "Edit" -> ""
                | caseName -> toKebabCase caseName

    /// Get HTTP method from a case's RouteAttribute (defaults based on case name)
    /// Convention: Create -> POST, Delete -> DELETE, Patch -> PATCH, else GET
    let private getMethod (case: UnionCaseInfo) : HttpMethod option =
        match getRouteAttr case with
        | Some attr -> Some(toHttpMethod attr.Method)
        | None ->
            match case.Name with
            | "Create" -> Some HttpMethod.Post
            | "Delete" -> Some HttpMethod.Delete
            | "Patch" -> Some HttpMethod.Patch
            | _otherCaseName -> Some HttpMethod.Get

    /// Recursively extract route info by walking the union value hierarchy
    /// Returns (HttpMethod option, path segments list)
    // fsharplint:disable-next-line FL0085 - not tail-recursive by design (processes results after recursive call)
    let rec private extractRouteInfo (value: obj) : HttpMethod option * string list =
        let valueType = value.GetType()

        if not (FSharpType.IsUnion(valueType)) then
            (None, [])
        else
            let case, fields = FSharpValue.GetUnionFields(value, valueType)
            let segment = getPathSegment case
            let method = getMethod case

            // Check if any field is a nested route union
            let nestedUnionField =
                case.GetFields() |> Array.tryFind (fun f -> isNestedRouteUnion f.PropertyType)

            match nestedUnionField with
            | Some fieldInfo ->
                // Find the corresponding field value
                let fieldIndex =
                    case.GetFields() |> Array.findIndex (fun f -> f.Name = fieldInfo.Name)

                let nestedValue = fields.[fieldIndex]
                let (nestedMethod, nestedSegments) = extractRouteInfo nestedValue

                // Prefer leaf values, fall back to parent
                let finalMethod =
                    match nestedMethod with
                    | Some m -> Some m
                    | None -> method

                // Build path: this segment + nested segments
                let pathSegments =
                    if segment = "" then
                        nestedSegments
                    else
                        [ segment ] @ nestedSegments

                (finalMethod, pathSegments)

            | None ->
                // Leaf case - no nested unions
                let pathSegments = if segment = "" then [] else [ segment ]
                (method, pathSegments)

    /// <summary>Full route metadata extracted via reflection.</summary>
    type RouteInfo =
        {
            /// <summary>The HTTP method for this route.</summary>
            Method: HttpMethod
            /// <summary>The URL path pattern for this route.</summary>
            Path: string
        }

    /// <summary>Gets full route metadata for a route value using reflection.</summary>
    /// <typeparam name="T">The route union type.</typeparam>
    /// <param name="route">The route value to inspect.</param>
    /// <returns><c>Some info</c> with method and path, or <c>None</c> if the route has no method.</returns>
    let tryRouteInfo (route: 'T) : RouteInfo option =
        let (methodOpt, segments) = extractRouteInfo (box route)

        match methodOpt with
        | Some method ->
            let path =
                if segments.IsEmpty then
                    "/"
                else
                    "/" + String.concat "/" segments

            Some { Method = method; Path = path }
        | None -> None

    /// <summary>Gets full route metadata for a route value using reflection.</summary>
    /// <typeparam name="T">The route union type.</typeparam>
    /// <param name="route">The route value to inspect.</param>
    /// <returns>A <see cref="RouteInfo"/> with method and path.</returns>
    /// <exception cref="System.Exception">Thrown if the route case has no HTTP method.</exception>
    let routeInfo (route: 'T) : RouteInfo =
        match tryRouteInfo route with
        | Some info -> info
        | None ->
            let case, _ = FSharpValue.GetUnionFields(route, typeof<'T>)
            failwithf "Route case '%s' is missing [<Route(...)>] attribute" case.Name

    /// <summary>Gets HTTP method and path pattern as a tuple for a route value.</summary>
    /// <typeparam name="T">The route union type.</typeparam>
    /// <param name="route">The route value to inspect.</param>
    /// <returns>A tuple of (HttpMethod, path pattern string).</returns>
    let routeTuple (route: 'T) : HttpMethod * string =
        let info = routeInfo route
        (info.Method, info.Path)

    /// Recursively extract a concrete link by walking the union value hierarchy
    /// and substituting actual field values into the path.
    // fsharplint:disable-next-line FL0085 - not tail-recursive by design
    let rec private extractLink (value: obj) : string list =
        let valueType = value.GetType()

        if not (FSharpType.IsUnion(valueType)) then
            []
        else
            let case, fieldValues = FSharpValue.GetUnionFields(value, valueType)
            let fieldInfos = case.GetFields()

            // Get the path segment pattern for this case
            let segmentPattern = getPathSegment case

            // Check if any field is a nested route union
            let nestedUnionFieldIndex =
                fieldInfos |> Array.tryFindIndex (fun f -> isNestedRouteUnion f.PropertyType)

            // Unwrap single-case wrapper types to get the inner value for string conversion
            let unwrapValue (v: obj) : obj =
                if isNull v then
                    v
                else
                    let vType = v.GetType()

                    if isSingleCaseWrapper vType then
                        let cases = FSharpType.GetUnionCases(vType)
                        let _, fields = FSharpValue.GetUnionFields(v, vType)
                        if fields.Length > 0 then fields.[0] else v
                    else
                        v

            // Substitute field values into the segment pattern
            // Only include route path fields (exclude nested unions, preconditions, query params)
            let segment =
                if String.IsNullOrEmpty(segmentPattern) then
                    ""
                else
                    fieldInfos
                    |> Array.indexed
                    |> Array.filter (fun (i, f) -> not (isNonRouteField f) && i < fieldValues.Length)
                    |> Array.fold
                        (fun (seg: string) (i, f) ->
                            let value = fieldValues.[i]

                            let valueStr =
                                match unwrapValue value with
                                | null -> ""
                                | v -> v.ToString()

                            seg.Replace("{" + f.Name + "}", valueStr))
                        segmentPattern

            match nestedUnionFieldIndex with
            | Some idx ->
                let nestedValue = fieldValues.[idx]
                let nestedSegments = extractLink nestedValue

                if segment = "" then
                    nestedSegments
                else
                    [ segment ] @ nestedSegments

            | None -> if segment = "" then [] else [ segment ]

    /// <summary>Generates a concrete URL path from a route value by substituting actual field values.</summary>
    /// <typeparam name="T">The route union type.</typeparam>
    /// <param name="route">The route value with actual parameter values.</param>
    /// <returns>A URL path with parameters substituted.</returns>
    /// <example>
    /// <code>
    /// let postId = PostId (Guid.Parse "abc-123")
    /// let url = RouteReflection.link (Posts (Detail postId))
    /// // returns "/posts/abc-123"
    /// </code>
    /// </example>
    let link (route: 'T) : string =
        let segments = extractLink (box route)

        if segments.IsEmpty then
            "/"
        else
            "/" + String.concat "/" segments

    // =========================================================================
    // Route enumeration via reflection
    // =========================================================================

    /// Get a default value for a type (for parameterized route cases)
    let private getDefaultValue (t: Type) : obj =
        if t = typeof<Guid> then box Guid.Empty
        elif t = typeof<string> then box ""
        elif t = typeof<int> then box 0
        elif t = typeof<int64> then box 0L
        elif t.IsValueType then Activator.CreateInstance(t)
        else null

    /// Enumerate all values of a union type, recursively expanding nested route unions.
    /// Returns obj list that can be cast to the appropriate type.
    // fsharplint:disable-next-line FL0085 - not tail-recursive by design (collects results via List.collect)
    let rec private enumerateUnionValues (unionType: Type) : obj list =
        if not (FSharpType.IsUnion(unionType)) then
            []
        else
            FSharpType.GetUnionCases(unionType)
            |> Array.toList
            |> List.collect (fun case ->
                let fields = case.GetFields()

                // Check if this case has a nested route union field
                let nestedRouteField =
                    fields |> Array.tryFind (fun f -> isNestedRouteUnion f.PropertyType)

                match nestedRouteField with
                | Some fieldInfo ->
                    // Recursively enumerate the nested union
                    let nestedValues = enumerateUnionValues fieldInfo.PropertyType

                    // For each nested value, wrap it in this case
                    nestedValues
                    |> List.map (fun nestedValue ->
                        let args =
                            fields
                            |> Array.map (fun f ->
                                if f.Name = fieldInfo.Name then
                                    nestedValue
                                else
                                    getDefaultValue f.PropertyType)

                        FSharpValue.MakeUnion(case, args))

                | None ->
                    // Leaf case - no nested unions, just create with default args
                    let args = fields |> Array.map (fun f -> getDefaultValue f.PropertyType)
                    [ FSharpValue.MakeUnion(case, args) ])

    /// <summary>Enumerates all route cases for a route union type.</summary>
    /// <typeparam name="TRoute">The route union type to enumerate.</typeparam>
    /// <returns>A list of all possible route values, with parameterized routes using default values.</returns>
    /// <remarks>
    /// Parameterized routes are created with default values: <c>Guid.Empty</c>, <c>""</c>, <c>0</c>, etc.
    /// Useful for generating endpoint lists or documentation.
    /// </remarks>
    /// <example>
    /// <code>
    /// let allRoutes = RouteReflection.allRoutes&lt;Route&gt;()
    /// for route in allRoutes do
    ///     let info = RouteReflection.routeInfo route
    ///     printfn "%A %s" info.Method info.Path
    /// </code>
    /// </example>
    let allRoutes<'TRoute> () : 'TRoute list =
        enumerateUnionValues typeof<'TRoute> |> List.map (fun o -> o :?> 'TRoute)

    // =========================================================================
    // Route validation
    // =========================================================================

    /// Valid characters for route path segments (excluding parameter placeholders)
    let private validPathChars =
        Set.ofList ([ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '0' .. '9' ] @ [ '-'; '_'; '/'; '.' ])

    /// Validates a path segment for invalid characters
    let private validatePathChars (path: string) : string list =
        let invalidChars =
            path.Replace("{", "").Replace("}", "")
            |> Seq.filter (fun c -> not (Set.contains c validPathChars))
            |> Seq.distinct
            |> Seq.toList

        if invalidChars.IsEmpty then
            []
        else
            let charsStr = invalidChars |> List.map string |> String.concat ", "
            [ $"Invalid characters in path '{path}': {charsStr}" ]

    /// Validates that path parameter braces are balanced
    let private validateBalancedBraces (path: string) : string list =
        let rec check chars depth =
            match chars with
            | [] -> depth = 0
            | '{' :: rest -> check rest (depth + 1)
            | '}' :: rest when depth > 0 -> check rest (depth - 1)
            | '}' :: _ -> false // Closing brace without opening
            | _ :: rest -> check rest depth

        if check (Seq.toList path) 0 then
            []
        else
            [ $"Unbalanced braces in path '{path}'" ]

    /// Extracts parameter names from a path pattern (e.g., "{id}/{slug}" -> ["id"; "slug"])
    let private extractPathParams (path: string) : string list =
        System.Text.RegularExpressions.Regex.Matches(path, @"\{([^}]+)\}")
        |> Seq.cast<System.Text.RegularExpressions.Match>
        |> Seq.map (fun m -> m.Groups.[1].Value)
        |> Seq.toList

    /// Validates that path params don't have duplicates
    let private validateNoDuplicateParams (path: string) : string list =
        let params' = extractPathParams path

        let duplicates =
            params'
            |> List.countBy id
            |> List.filter (fun (_, count) -> count > 1)
            |> List.map fst

        if duplicates.IsEmpty then
            []
        else
            let dupsStr = duplicates |> String.concat ", "
            [ $"Duplicate path parameters in '{path}': {dupsStr}" ]

    /// Gets the route path field names for a case (excludes nested unions, preconditions, query params)
    let private getRoutePathFieldNames (case: UnionCaseInfo) : string list =
        case.GetFields()
        |> Array.filter (fun f -> not (isNonRouteField f))
        |> Array.map (fun f -> f.Name)
        |> Array.toList

    /// Validates that path params match field names for a case
    let private validatePathParamsMatchFields (case: UnionCaseInfo) (path: string) : string list =
        let pathParams = extractPathParams path |> Set.ofList
        let fieldNames = getRoutePathFieldNames case |> Set.ofList

        let missingInFields = Set.difference pathParams fieldNames
        let missingInPath = Set.difference fieldNames pathParams

        let errors = ResizeArray()

        if not (Set.isEmpty missingInFields) then
            let missingStr = missingInFields |> String.concat ", "
            errors.Add($"Path params not found in fields for '{case.Name}': {missingStr}")

        if not (Set.isEmpty missingInPath) then
            let missingStr = missingInPath |> String.concat ", "
            errors.Add($"Fields not in path for '{case.Name}': {missingStr}")

        errors |> Seq.toList

    /// Counts nested route unions in a case's fields
    let private countNestedRouteUnions (case: UnionCaseInfo) : int =
        case.GetFields()
        |> Array.filter (fun f -> isNestedRouteUnion f.PropertyType)
        |> Array.length

    /// Validates a single union case
    let private validateCase (case: UnionCaseInfo) : string list =
        let path = getPathSegment case
        let errors = ResizeArray()

        // Check for multiple nested route unions
        let nestedCount = countNestedRouteUnions case

        if nestedCount > 1 then
            errors.Add($"Case '{case.Name}' has {nestedCount} nested route unions (max 1 supported)")

        // Validate path syntax
        errors.AddRange(validatePathChars path)
        errors.AddRange(validateBalancedBraces path)
        errors.AddRange(validateNoDuplicateParams path)

        // Validate path params match fields (only if path has params)
        if path.Contains("{") then
            errors.AddRange(validatePathParamsMatchFields case path)

        errors |> Seq.toList

    /// Recursively validates all union cases in a route type
    let rec private validateUnionType (unionType: Type) : string list =
        if not (FSharpType.IsUnion(unionType)) then
            []
        else
            FSharpType.GetUnionCases(unionType)
            |> Array.toList
            |> List.collect (fun case ->
                let caseErrors = validateCase case

                // Recursively validate nested route unions
                let nestedErrors =
                    case.GetFields()
                    |> Array.filter (fun f -> isNestedRouteUnion f.PropertyType)
                    |> Array.toList
                    |> List.collect (fun f -> validateUnionType f.PropertyType)

                caseErrors @ nestedErrors)

    /// <summary>Validates route structure including paths, field types, and constraints.</summary>
    /// <typeparam name="Route">The route union type to validate.</typeparam>
    /// <returns><c>Ok ()</c> if valid, <c>Error</c> with list of issues if invalid.</returns>
    /// <remarks>
    /// <para>Validates the following:</para>
    /// <list type="bullet">
    ///   <item><description>Invalid characters in paths</description></item>
    ///   <item><description>Unbalanced braces in path patterns</description></item>
    ///   <item><description>Duplicate path parameters</description></item>
    ///   <item><description>Path parameters matching field names</description></item>
    ///   <item><description>Multiple nested route unions (max 1 supported)</description></item>
    /// </list>
    /// <para>Called automatically by <see cref="endpoints"/>.</para>
    /// </remarks>
    /// <example>
    /// <code>
    /// match RouteReflection.validateStructure&lt;Route&gt;() with
    /// | Ok () -> printfn "Routes are valid"
    /// | Error errors -> errors |> List.iter (printfn "Error: %s")
    /// </code>
    /// </example>
    let validateStructure<'Route> () : Result<unit, string list> =
        let errors = validateUnionType typeof<'Route>

        if errors.IsEmpty then Ok() else Error errors

    // =========================================================================
    // Falco integration
    // =========================================================================

    /// <summary>Converts an <see cref="HttpMethod"/> to Falco's route function.</summary>
    /// <param name="method">The HTTP method to convert.</param>
    /// <returns>The corresponding Falco route function (get, post, put, delete, patch, or any).</returns>
    let toFalcoMethod (method: HttpMethod) =
        match method with
        | HttpMethod.Get -> get
        | HttpMethod.Post -> post
        | HttpMethod.Put -> put
        | HttpMethod.Delete -> delete
        | HttpMethod.Patch -> patch
        | HttpMethod.Any -> any

    /// <summary>Generates Falco endpoints from a route handler function.</summary>
    /// <typeparam name="TRoute">The route union type.</typeparam>
    /// <param name="routeHandler">A function that takes a route value and returns an HTTP handler.</param>
    /// <returns>A list of Falco <c>HttpEndpoint</c> values ready for use with <c>app.UseFalco</c>.</returns>
    /// <exception cref="System.Exception">Thrown if route validation fails (invalid paths, missing fields, etc.).</exception>
    /// <remarks>
    /// <para>Enumerates all routes of the given type and maps each to an HttpEndpoint.</para>
    /// <para>Automatically validates route structure at startup via <see cref="validateStructure"/>.</para>
    /// </remarks>
    /// <example>
    /// <code>
    /// let routeHandler route =
    ///     match route with
    ///     | Home -> Response.ofPlainText "home"
    ///     | Posts p -> postHandler p
    ///
    /// let endpoints = RouteReflection.endpoints routeHandler
    /// app.UseFalco(endpoints) |> ignore
    /// </code>
    /// </example>
    let endpoints (routeHandler: 'TRoute -> HttpHandler) : HttpEndpoint list =
        // Validate route structure at startup
        match validateStructure<'TRoute> () with
        | Error errors ->
            let errorMsg = errors |> String.concat "\n  - "
            failwith $"Route validation failed:\n  - {errorMsg}"
        | Ok() -> ()

        allRoutes<'TRoute> ()
        |> List.map (fun route ->
            let info = routeInfo route
            let handler = routeHandler route
            toFalcoMethod info.Method info.Path handler)
