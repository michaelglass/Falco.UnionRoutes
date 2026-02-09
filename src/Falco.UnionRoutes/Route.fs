namespace Falco.UnionRoutes

open System
open System.Text.RegularExpressions
open Falco
open Falco.Routing
open Microsoft.AspNetCore.Http
open Microsoft.FSharp.Reflection

// =============================================================================
// Route Attributes and Types
// =============================================================================

/// <summary>HTTP methods for route attributes.</summary>
/// <remarks>Used with <see cref="T:Falco.UnionRoutes.RouteAttribute"/> to specify the HTTP method for a route case.</remarks>
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

// =============================================================================
// Route Module
// =============================================================================

/// <summary>Type-safe routing with discriminated unions.</summary>
/// <remarks>
/// <para>The Route module provides functions for:</para>
/// <list type="bullet">
///   <item><description>Generating Falco endpoints from route unions</description></item>
///   <item><description>Creating type-safe links from route values</description></item>
///   <item><description>Hydrating routes with values from HTTP context</description></item>
///   <item><description>Validating route structure and preconditions</description></item>
/// </list>
/// </remarks>
/// <example>
/// <code>
/// type PostRoute =
///     | List
///     | Show of id: Guid
///     | Create of PreCondition&lt;UserId&gt;
///
/// let config: EndpointConfig&lt;AppError&gt; = {
///     Preconditions = [ Extractor.precondition&lt;UserId, _&gt; authExtractor ]
///     Parsers = []
///     MakeError = fun msg -> BadRequest msg
///     CombineErrors = List.head
///     ToErrorResponse = fun e -> Response.ofPlainText (string e)
/// }
///
/// let endpoints = Route.endpoints config handleRoute
/// </code>
/// </example>
[<RequireQualifiedAccess>]
module Route =

    // =========================================================================
    // Internal helpers (from RouteReflection)
    // =========================================================================

    // Precompiled regex for better performance during route enumeration
    let private kebabCaseRegex =
        Regex(@"([a-z])([A-Z])|([A-Z]+)([A-Z][a-z])", RegexOptions.Compiled)

    /// Converts PascalCase to kebab-case. Internal helper for path generation.
    let internal toKebabCase (s: string) =
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

    /// Converts RouteMethod enum to HttpMethod DU. Internal helper.
    let internal toHttpMethod (rm: RouteMethod) : HttpMethod =
        match rm with
        | RouteMethod.Get -> HttpMethod.Get
        | RouteMethod.Post -> HttpMethod.Post
        | RouteMethod.Put -> HttpMethod.Put
        | RouteMethod.Delete -> HttpMethod.Delete
        | RouteMethod.Patch -> HttpMethod.Patch
        | RouteMethod.Any -> HttpMethod.Any
        | unknown -> failwith $"Unknown RouteMethod: {int unknown}"

    /// Gets the RouteAttribute from a union case, if present. Internal helper.
    let private getRouteAttr (case: UnionCaseInfo) : RouteAttribute option =
        case.GetCustomAttributes(typeof<RouteAttribute>)
        |> Array.tryHead
        |> Option.map (fun a -> a :?> RouteAttribute)

    /// Get the Path from a RouteAttribute as Option (handles CLR null)
    let private getAttrPath (attr: RouteAttribute) : string option =
        match attr.Path with
        | null -> None
        | path -> Some path

    /// Check if a type is PreCondition<'T> (precondition marker - should not be in route path)
    let private isPreconditionType (t: Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<PreCondition<_>>

    /// Check if a type is OverridablePreCondition<'T> (skippable precondition marker - should not be in route path)
    let private isOptionalPreconditionType (t: Type) =
        t.IsGenericType
        && t.GetGenericTypeDefinition() = typedefof<OverridablePreCondition<_>>

    /// Check if a type is QueryParam<'T> (query parameter - should not be in route path)
    let private isQueryType (t: Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<QueryParam<_>>

    /// Check if a type is QueryParam<'T> option (optional query parameter - should not be in route path)
    let private isOptionalQueryType (t: Type) =
        t.IsGenericType
        && t.GetGenericTypeDefinition() = typedefof<option<_>>
        && t.GetGenericArguments().[0].IsGenericType
        && t.GetGenericArguments().[0].GetGenericTypeDefinition() = typedefof<QueryParam<_>>

    /// Supported primitive types for route/query extraction
    let private supportedPrimitives =
        [ typeof<Guid>; typeof<string>; typeof<int>; typeof<int64>; typeof<bool> ]

    /// Check if a type is a single-case DU wrapper for a primitive (e.g., PostId of Guid)
    let private isSingleCaseWrapper (t: Type) =
        FSharpType.IsUnion(t)
        && let cases = FSharpType.GetUnionCases(t) in

           cases.Length = 1
           && cases.[0].GetFields().Length = 1
           && supportedPrimitives |> List.contains (cases.[0].GetFields().[0].PropertyType)

    /// Check if a type is a nested route union (for hierarchy traversal)
    let private isNestedRouteUnion (t: Type) =
        FSharpType.IsUnion(t)
        && t <> typeof<string>
        && not (t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>)
        && not (isPreconditionType t)
        && not (isOptionalPreconditionType t)
        && not (isQueryType t)
        && not (isOptionalQueryType t)
        && not (isSingleCaseWrapper t)

    /// Check if a field should be excluded from route path
    let private isNonRouteField (f: Reflection.PropertyInfo) =
        isNestedRouteUnion f.PropertyType
        || isPreconditionType f.PropertyType
        || isOptionalPreconditionType f.PropertyType
        || isQueryType f.PropertyType
        || isOptionalQueryType f.PropertyType

    // =========================================================================
    // Path and method inference
    // =========================================================================

    /// Does this case name produce an empty segment? RESTful names like
    /// List, Show, Create etc. are "invisible" in the URL path.
    let private isEmptySegmentName (caseName: string) : bool =
        match caseName with
        | "Root"
        | "List"
        | "Show"
        | "Member"
        | "Create"
        | "Delete"
        | "Patch" -> true
        | _ -> false

    /// Infer path segment from case fields
    let private inferPathFromFields (case: UnionCaseInfo) : string option =
        let fields = case.GetFields()

        if fields.Length = 0 then
            None
        elif fields.Length = 1 && isNestedRouteUnion fields.[0].PropertyType then
            None
        else
            let pathFields = fields |> Array.filter (fun f -> not (isNonRouteField f))

            if pathFields.Length = 0 then
                None
            else
                let paramPath =
                    pathFields |> Array.map (fun f -> "{" + f.Name + "}") |> String.concat "/"

                match isEmptySegmentName case.Name with
                | true -> Some paramPath
                | false -> Some(toKebabCase case.Name + "/" + paramPath)

    /// Get path segment from a case
    let private getPathSegment (case: UnionCaseInfo) : string =
        match getRouteAttr case |> Option.bind getAttrPath with
        | Some path -> path
        | None ->
            match inferPathFromFields case with
            | Some path -> path
            | None ->
                if isEmptySegmentName case.Name then
                    ""
                else
                    toKebabCase case.Name

    /// Get HTTP method from a case's RouteAttribute
    let private getMethod (case: UnionCaseInfo) : HttpMethod =
        match getRouteAttr case with
        | Some attr -> toHttpMethod attr.Method
        | None ->
            match case.Name with
            | "Create" -> HttpMethod.Post
            | "Delete" -> HttpMethod.Delete
            | "Patch" -> HttpMethod.Patch
            | _ -> HttpMethod.Get

    /// Recursively extract route info
    // fsharplint:disable-next-line FL0085
    let rec private extractRouteInfo (value: obj) : HttpMethod * string list =
        let valueType = value.GetType()
        let case, fields = FSharpValue.GetUnionFields(value, valueType)
        let segment = getPathSegment case
        let method = getMethod case

        let nestedUnionField =
            case.GetFields() |> Array.tryFind (fun f -> isNestedRouteUnion f.PropertyType)

        match nestedUnionField with
        | Some fieldInfo ->
            let fieldIndex =
                case.GetFields() |> Array.findIndex (fun f -> f.Name = fieldInfo.Name)

            let nestedValue = fields.[fieldIndex]
            let (nestedMethod, nestedSegments) = extractRouteInfo nestedValue

            let pathSegments =
                if segment = "" then
                    nestedSegments
                else
                    segment :: nestedSegments

            (nestedMethod, pathSegments)

        | None ->
            let pathSegments = if segment = "" then [] else [ segment ]
            (method, pathSegments)

    // =========================================================================
    // Public API - Route Info
    // =========================================================================

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
    /// <returns>A <see cref="T:Falco.UnionRoutes.RouteInfo"/> with method and path.</returns>
    /// <example>
    /// <code>
    /// let routeInfo = Route.info (Posts (Detail (PostId Guid.Empty)))
    /// // routeInfo.Method = HttpMethod.Get
    /// // routeInfo.Path = "/posts/{id}"
    /// </code>
    /// </example>
    let info (route: 'T) : RouteInfo =
        let (method, segments) = extractRouteInfo (box route)

        let path =
            if segments.IsEmpty then
                "/"
            else
                "/" + String.concat "/" segments

        { Method = method; Path = path }

    // =========================================================================
    // Public API - Links
    // =========================================================================

    /// Recursively extract a concrete link
    // fsharplint:disable-next-line FL0085
    let rec private extractLink (value: obj) : string list =
        let valueType = value.GetType()

        if not (FSharpType.IsUnion(valueType)) then
            []
        else
            let case, fieldValues = FSharpValue.GetUnionFields(value, valueType)
            let fieldInfos = case.GetFields()
            let segmentPattern = getPathSegment case

            let nestedUnionFieldIndex =
                fieldInfos |> Array.tryFindIndex (fun f -> isNestedRouteUnion f.PropertyType)

            let unwrapValue (v: obj) : obj =
                if isNull v then
                    v
                else
                    let vType = v.GetType()

                    if isSingleCaseWrapper vType then
                        let _, fields = FSharpValue.GetUnionFields(v, vType)
                        if fields.Length > 0 then fields.[0] else v
                    else
                        v

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
                    segment :: nestedSegments

            | None -> if segment = "" then [] else [ segment ]

    /// <summary>Generates a concrete URL path from a route value by substituting actual field values.</summary>
    /// <typeparam name="T">The route union type.</typeparam>
    /// <param name="route">The route value with actual parameter values.</param>
    /// <returns>A URL path with parameters substituted.</returns>
    /// <example>
    /// <code>
    /// let postId = PostId (Guid.Parse "abc-123")
    /// let url = Route.link (Posts (Detail postId))
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
    // Public API - Route Enumeration
    // =========================================================================

    /// Get a default value for a type
    let rec private getDefaultValue (t: Type) : obj =
        if t = typeof<Guid> then
            box Guid.Empty
        elif t = typeof<string> then
            box ""
        elif t = typeof<int> then
            box 0
        elif t = typeof<int64> then
            box 0L
        elif isSingleCaseWrapper t then
            let case = FSharpType.GetUnionCases(t).[0]
            let innerDefault = getDefaultValue (case.GetFields().[0].PropertyType)
            FSharpValue.MakeUnion(case, [| innerDefault |])
        elif t.IsValueType then
            Activator.CreateInstance(t)
        else
            null

    /// Enumerate all values of a union type
    // fsharplint:disable-next-line FL0085
    let rec private enumerateUnionValues (unionType: Type) : obj list =
        if not (FSharpType.IsUnion(unionType)) then
            []
        else
            FSharpType.GetUnionCases(unionType)
            |> Array.toList
            |> List.collect (fun case ->
                let fields = case.GetFields()

                let nestedRouteField =
                    fields |> Array.tryFind (fun f -> isNestedRouteUnion f.PropertyType)

                match nestedRouteField with
                | Some fieldInfo ->
                    let nestedValues = enumerateUnionValues fieldInfo.PropertyType

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
                    let args = fields |> Array.map (fun f -> getDefaultValue f.PropertyType)
                    [ FSharpValue.MakeUnion(case, args) ])

    /// <summary>Enumerates all route cases for a route union type.</summary>
    /// <typeparam name="TRoute">The route union type to enumerate.</typeparam>
    /// <returns>A list of all possible route values, with parameterized routes using default values.</returns>
    /// <example>
    /// <code>
    /// let allRoutes = Route.allRoutes&lt;Route&gt;()
    /// for route in allRoutes do
    ///     let info = Route.info route
    ///     printfn "%A %s" info.Method info.Path
    /// </code>
    /// </example>
    let allRoutes<'TRoute> () : 'TRoute list =
        enumerateUnionValues typeof<'TRoute> |> List.map (fun o -> o :?> 'TRoute)

    // =========================================================================
    // Public API - Validation
    // =========================================================================

    /// Valid characters for route path segments
    let private validPathChars =
        Set.ofList ([ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '0' .. '9' ] @ [ '-'; '_'; '/'; '.' ])

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

    let private validateBalancedBraces (path: string) : string list =
        let rec check chars depth =
            match chars with
            | [] -> depth = 0
            | '{' :: rest -> check rest (depth + 1)
            | '}' :: rest when depth > 0 -> check rest (depth - 1)
            | '}' :: _ -> false
            | _ :: rest -> check rest depth

        if check (Seq.toList path) 0 then
            []
        else
            [ $"Unbalanced braces in path '{path}'" ]

    let private extractPathParams (path: string) : string list =
        Regex.Matches(path, @"\{([^}]+)\}")
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Groups.[1].Value)
        |> Seq.toList

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

    let private getRoutePathFieldNames (case: UnionCaseInfo) : string list =
        case.GetFields()
        |> Array.filter (fun f -> not (isNonRouteField f))
        |> Array.map (fun f -> f.Name)
        |> Array.toList

    let private validatePathParamsMatchFields (case: UnionCaseInfo) (path: string) : string list =
        let pathParams = extractPathParams path |> Set.ofList
        let fieldNames = getRoutePathFieldNames case |> Set.ofList

        let missingInFields = Set.difference pathParams fieldNames
        let missingInPath = Set.difference fieldNames pathParams

        [ if not (Set.isEmpty missingInFields) then
              let missingStr = missingInFields |> String.concat ", "
              $"Path params not found in fields for '{case.Name}': {missingStr}"

          if not (Set.isEmpty missingInPath) then
              let missingStr = missingInPath |> String.concat ", "
              $"Fields not in path for '{case.Name}': {missingStr}" ]

    let private countNestedRouteUnions (case: UnionCaseInfo) : int =
        case.GetFields()
        |> Array.filter (fun f -> isNestedRouteUnion f.PropertyType)
        |> Array.length

    let private validateCase (case: UnionCaseInfo) : string list =
        let path = getPathSegment case
        let nestedCount = countNestedRouteUnions case

        [ if nestedCount > 1 then
              $"Case '{case.Name}' has {nestedCount} nested route unions (max 1 supported)"

          yield! validatePathChars path
          yield! validateBalancedBraces path
          yield! validateNoDuplicateParams path

          if path.Contains("{") then
              yield! validatePathParamsMatchFields case path ]

    let rec private validateUnionType (unionType: Type) : string list =
        if not (FSharpType.IsUnion(unionType)) then
            []
        else
            FSharpType.GetUnionCases(unionType)
            |> Array.toList
            |> List.collect (fun case ->
                let caseErrors = validateCase case

                let nestedErrors =
                    case.GetFields()
                    |> Array.filter (fun f -> isNestedRouteUnion f.PropertyType)
                    |> Array.toList
                    |> List.collect (fun f -> validateUnionType f.PropertyType)

                caseErrors @ nestedErrors)

    /// <summary>Validates route structure including paths and field-to-parameter consistency.</summary>
    /// <typeparam name="Route">The route union type to validate.</typeparam>
    /// <returns><c>Ok ()</c> if valid, <c>Error</c> with list of issues if invalid.</returns>
    /// <example>
    /// <code>
    /// match Route.validateStructure&lt;Route&gt;() with
    /// | Ok () -> printfn "Routes are valid"
    /// | Error errors -> errors |> List.iter (printfn "Error: %s")
    /// </code>
    /// </example>
    let validateStructure<'Route> () : Result<unit, string list> =
        let errors = validateUnionType typeof<'Route>

        if errors.IsEmpty then Ok() else Error errors

    // =========================================================================
    // Cross-route uniqueness and ambiguity detection
    // =========================================================================

    /// A parsed path segment: either a literal string or a parameter placeholder.
    type private PathSegment =
        | Literal of string
        | Parameter

    /// Parse a route path template into typed segments.
    let private parsePathSegments (path: string) : PathSegment list =
        path.Split('/', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.map (fun seg -> if seg.Contains("{") then Parameter else Literal seg)

    /// Check if two parsed segment lists could match the same URL.
    let private routesOverlap (a: PathSegment list) (b: PathSegment list) : bool =
        a.Length = b.Length
        && List.forall2
            (fun sa sb ->
                match sa, sb with
                | Literal la, Literal lb -> la = lb
                | _ -> true)
            a
            b

    /// Check if route A is strictly more specific than route B.
    /// A is more specific when it has a literal wherever B has a literal,
    /// plus at least one position where A has a literal and B has a parameter.
    let private isMoreSpecific (a: PathSegment list) (b: PathSegment list) : bool =
        if a.Length <> b.Length then
            false
        else
            let pairs = List.zip a b

            let aHasLiteralWhereBHasParam =
                pairs
                |> List.exists (function
                    | Literal _, Parameter -> true
                    | _ -> false)

            let aHasParamWhereBHasLiteral =
                pairs
                |> List.exists (function
                    | Parameter, Literal _ -> true
                    | _ -> false)

            aHasLiteralWhereBHasParam && not aHasParamWhereBHasLiteral

    /// Recursively extract a qualified case path for error messages (e.g., "Posts.Detail").
    let rec private getRouteCasePath (value: obj) : string =
        let valueType = value.GetType()
        let case, fields = FSharpValue.GetUnionFields(value, valueType)

        let nestedField =
            case.GetFields()
            |> Array.tryFindIndex (fun f -> isNestedRouteUnion f.PropertyType)

        match nestedField with
        | Some idx -> $"{case.Name}.{getRouteCasePath fields.[idx]}"
        | None -> case.Name

    /// Check if two parsed segment lists are structurally identical (param names ignored).
    let private areDuplicateSegments (a: PathSegment list) (b: PathSegment list) : bool =
        a.Length = b.Length
        && List.forall2
            (fun sa sb ->
                match sa, sb with
                | Literal la, Literal lb -> la = lb
                | Parameter, Parameter -> true
                | _ -> false)
            a
            b

    /// Validate cross-route uniqueness and detect ambiguity.
    let private validateCrossRouteUniqueness (routeType: Type) : string list =
        let allValues = enumerateUnionValues routeType

        let routeData =
            allValues
            |> List.map (fun value ->
                let (method, segments) = extractRouteInfo value

                let path =
                    if segments.IsEmpty then
                        "/"
                    else
                        "/" + String.concat "/" segments

                let parsed = parsePathSegments path
                let casePath = getRouteCasePath value
                (method, path, parsed, casePath))

        let grouped = routeData |> List.groupBy (fun (method, _, _, _) -> method)

        grouped
        |> List.collect (fun (_method, routes) ->
            [ for i in 0 .. routes.Length - 2 do
                  for j in i + 1 .. routes.Length - 1 do
                      let (methodA, pathA, parsedA, caseA) = routes.[i]
                      let (_methodB, pathB, parsedB, caseB) = routes.[j]

                      if routesOverlap parsedA parsedB then
                          if areDuplicateSegments parsedA parsedB then
                              $"Duplicate route: '{caseA}' and '{caseB}' both resolve to {methodA} {pathA}"
                          elif not (isMoreSpecific parsedA parsedB) && not (isMoreSpecific parsedB parsedA) then
                              $"Ambiguous routes: '{caseA}' ({methodA} {pathA}) and '{caseB}' ({methodA} {pathB}) overlap with no clear specificity winner" ])

    /// Validates that no two routes resolve to the same method+path and detects ambiguous overlaps.
    let internal validateUniqueness<'Route> () : Result<unit, string list> =
        let errors = validateCrossRouteUniqueness typeof<'Route>

        if errors.IsEmpty then Ok() else Error errors

    // =========================================================================
    // Public API - Falco Integration
    // =========================================================================

    /// Converts HttpMethod to Falco's route function. Internal helper.
    let internal toFalcoMethod (method: HttpMethod) =
        match method with
        | HttpMethod.Get -> get
        | HttpMethod.Post -> post
        | HttpMethod.Put -> put
        | HttpMethod.Delete -> delete
        | HttpMethod.Patch -> patch
        | HttpMethod.Any -> any

    // =========================================================================
    // Hydration - Internal helpers
    // =========================================================================

    /// Detects if a type is PreCondition<'T>.
    let private tryGetPreInfo (t: Type) : Type option =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<PreCondition<_>> then
            Some(t.GetGenericArguments().[0])
        else
            None

    /// Detects if a type is OverridablePreCondition<'T>.
    let private tryGetOptPreInfo (t: Type) : Type option =
        if
            t.IsGenericType
            && t.GetGenericTypeDefinition() = typedefof<OverridablePreCondition<_>>
        then
            Some(t.GetGenericArguments().[0])
        else
            None

    /// Detects if a type is a single-case DU wrapping another type.
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

    let private isTupleType (t: Type) = FSharpType.IsTuple t
    let private isRecordType (t: Type) = FSharpType.IsRecord t

    let private isAnonymousRecordType (t: Type) =
        t.Name.StartsWith("<>f__AnonymousType")
        || (t.GetCustomAttributes(typeof<CompilationMappingAttribute>, false)
            |> Array.exists (fun attr ->
                (attr :?> CompilationMappingAttribute).SourceConstructFlags = SourceConstructFlags.RecordType)
            && t.Name.Contains("@"))

    let private tryGetOptionInfo (t: Type) : Type option =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
            Some(t.GetGenericArguments().[0])
        else
            None

    let private tryGetQueryInfo (t: Type) : Type option =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<QueryParam<_>> then
            Some(t.GetGenericArguments().[0])
        else
            None

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
    let rec private findLeafCaseInfo (value: obj) : UnionCaseInfo =
        let valueType = value.GetType()

        if not (FSharpType.IsUnion(valueType)) then
            failwith $"Expected union type, got {valueType.Name}"
        else
            let caseInfo, fieldValues = FSharpValue.GetUnionFields(value, valueType)
            let fields = caseInfo.GetFields()

            let nestedUnionFieldIndex =
                fields |> Array.tryFindIndex (fun f -> isNestedRouteUnion f.PropertyType)

            match nestedUnionFieldIndex with
            | Some idx ->
                let nestedValue = fieldValues.[idx]
                findLeafCaseInfo nestedValue
            | None -> caseInfo

    let private boxSome (innerType: Type) (value: obj) : obj =
        let someCase =
            FSharpType.GetUnionCases(typedefof<option<_>>.MakeGenericType(innerType))
            |> Array.find (fun c -> c.Name = "Some")

        FSharpValue.MakeUnion(someCase, [| value |])

    let private boxNone (innerType: Type) : obj =
        let noneCase =
            FSharpType.GetUnionCases(typedefof<option<_>>.MakeGenericType(innerType))
            |> Array.find (fun c -> c.Name = "None")

        FSharpValue.MakeUnion(noneCase, [||])

    type private ParamSource =
        | RouteParam
        | QueryString

        member this.Name =
            match this with
            | RouteParam -> "route"
            | QueryString -> "query"

    let private isSupportedPrimitive (t: Type) = supportedPrimitives |> List.contains t

    let private tryParsePrimitive (primitiveType: Type) (value: string) (fieldName: string) (source: ParamSource) =
        let sourceName = source.Name

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

    let private getRouteString (ctx: HttpContext) (fieldName: string) : string =
        // TODO: go back to GetString when https://github.com/falcoframework/Falco/pull/150 gets released
        match ctx.Request.RouteValues.TryGetValue(fieldName) with
        | true, value when not (isNull value) -> value.ToString()
        | true, _nullValue -> ""
        | false, _ -> ""

    let private tryExtractPrimitive
        (source: ParamSource)
        (fieldName: string)
        (primitiveType: Type)
        (ctx: HttpContext)
        : Result<obj, string> option =

        let value =
            match source with
            | RouteParam -> getRouteString ctx fieldName
            | QueryString -> (Request.getQuery ctx).GetString fieldName

        if String.IsNullOrEmpty(value) then
            None
        else
            match tryParsePrimitive primitiveType value fieldName source with
            | Some result -> Some result
            | None -> Some(Error $"Unsupported primitive type for {source.Name}: {primitiveType.Name}")

    let private tryCustomParsers
        (parsers: FieldParser list)
        (fieldName: string)
        (fieldType: Type)
        (ctx: HttpContext)
        : Result<obj, string> option =
        parsers
        |> List.tryFind (fun p -> p.ForType = fieldType)
        |> Option.map (fun parser ->
            let value = getRouteString ctx fieldName

            if String.IsNullOrEmpty(value) then
                Error $"Missing route parameter: {fieldName}"
            else
                parser.Parse value)

    let private extractRequired
        (parsers: FieldParser list)
        (fieldName: string)
        (fieldType: Type)
        (ctx: HttpContext)
        : Result<obj, string> =

        match tryCustomParsers parsers fieldName fieldType ctx with
        | Some result -> result
        | None ->
            match tryGetQueryInfo fieldType with
            | Some innerType ->
                match tryExtractPrimitive QueryString fieldName innerType ctx with
                | Some(Ok v) ->
                    let queryCase = FSharpType.GetUnionCases(fieldType).[0]
                    Ok(FSharpValue.MakeUnion(queryCase, [| v |]))
                | Some(Error msg) -> Error msg
                | None -> Error $"Missing query parameter: {fieldName}"
            | None ->
                if isSupportedPrimitive fieldType then
                    tryExtractPrimitive RouteParam fieldName fieldType ctx
                    |> Option.defaultValue (Error $"Missing route parameter: {fieldName}")
                else
                    match tryGetWrapperInfo fieldType with
                    | Some(innerType, wrapFn) ->
                        tryExtractPrimitive RouteParam fieldName innerType ctx
                        |> Option.defaultValue (Error $"Missing route parameter: {fieldName}")
                        |> Result.map wrapFn
                    | None ->
                        if isTupleType fieldType then
                            failwith
                                $"Route: Tuple types are not supported for field '{fieldName}'. Use named fields instead: '| Case of a: Guid * b: int'"
                        elif isRecordType fieldType then
                            failwith
                                $"Route: Record types are not yet supported for field '{fieldName}'. Use named fields directly on the union case."
                        elif isAnonymousRecordType fieldType then
                            failwith
                                $"Route: Anonymous record types are not supported for field '{fieldName}'. Use named fields directly on the union case."
                        else
                            failwith
                                $"Route: Don't know how to extract field '{fieldName}' of type {fieldType.Name}. Supported: Guid, string, int, int64, bool, QueryParam<T>, PreCondition<T>, or single-case wrapper DUs."

    let private extractField
        (parsers: FieldParser list)
        (fieldName: string)
        (fieldType: Type)
        (ctx: HttpContext)
        : Result<obj, string> =

        match tryGetOptionInfo fieldType with
        | Some innerType ->
            match tryGetQueryInfo innerType with
            | Some queryInnerType ->
                match tryExtractPrimitive QueryString fieldName queryInnerType ctx with
                | Some(Ok v) ->
                    let queryCase = FSharpType.GetUnionCases(innerType).[0]
                    let queryVal = FSharpValue.MakeUnion(queryCase, [| v |])
                    Ok(boxSome innerType queryVal)
                | Some(Error msg) -> Error msg
                | None -> Ok(boxNone innerType)
            | None ->
                failwith
                    $"Route: Option types only supported for QueryParam<'T>. Use QueryParam<{innerType.Name}> option for optional query params."
        | None -> extractRequired parsers fieldName fieldType ctx

    let private findPrecondition (preconditions: PreconditionExtractor<'E> list) (fieldType: Type) =
        preconditions |> List.tryFind (fun p -> p.ForType = fieldType)

    let private createSkippedOptPreValue (optPreType: Type) : obj =
        let innerType = optPreType.GetGenericArguments().[0]
        let defaultValue = getDefaultValue innerType
        let optPreCase = FSharpType.GetUnionCases(optPreType).[0]
        FSharpValue.MakeUnion(optPreCase, [| defaultValue |])

    [<NoComparison; NoEquality>]
    type private FieldResult<'Error> =
        | FromPrecondition of Result<obj, 'Error>
        | FromExtraction of Result<obj, string>

    let rec private collectPreconditionTypes (unionType: Type) : Type list =
        if not (FSharpType.IsUnion(unionType)) then
            []
        else
            FSharpType.GetUnionCases(unionType)
            |> Array.toList
            |> List.collect (fun case ->
                let fields = case.GetFields()

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

                let nestedTypes =
                    fields
                    |> Array.filter (fun f -> isNestedRouteUnion f.PropertyType)
                    |> Array.toList
                    |> List.collect (fun f -> collectPreconditionTypes f.PropertyType)

                preconditionTypes @ nestedTypes)
            |> List.distinct

    let private formatTypeName (t: Type) : string =
        if t.IsGenericType then
            let baseName = t.Name.Substring(0, t.Name.IndexOf('`'))

            let args =
                t.GetGenericArguments() |> Array.map (fun a -> a.Name) |> String.concat ", "

            $"{baseName}<{args}>"
        else
            t.Name

    // =========================================================================
    // Public API - Precondition Validation
    // =========================================================================

    /// <summary>Validates that all <c>PreCondition&lt;T&gt;</c> and <c>OverridablePreCondition&lt;T&gt;</c> fields have registered extractors.</summary>
    /// <typeparam name="Route">The route union type to validate.</typeparam>
    /// <typeparam name="E">The error type used by precondition extractors.</typeparam>
    /// <param name="preconditions">The list of registered precondition extractors.</param>
    /// <returns><c>Ok ()</c> if all types are covered, <c>Error</c> with list of missing types if invalid.</returns>
    /// <example>
    /// <code>
    /// let preconditions = [ Extractor.precondition authExtractor; Extractor.overridablePrecondition adminExtractor ]
    /// match Route.validatePreconditions&lt;Route, AppError&gt; preconditions with
    /// | Ok () -> ()
    /// | Error errors -> failwith (String.concat "\n" errors)
    /// </code>
    /// </example>
    let validatePreconditions<'Route, 'E> (preconditions: PreconditionExtractor<'E> list) : Result<unit, string list> =
        let requiredTypes = collectPreconditionTypes typeof<'Route>
        let registeredTypes = preconditions |> List.map (fun p -> p.ForType)

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
    /// <typeparam name="E">The error type used by precondition extractors.</typeparam>
    /// <param name="preconditions">The list of registered precondition extractors.</param>
    /// <returns><c>Ok ()</c> if valid, <c>Error</c> with list of all issues if invalid.</returns>
    /// <example>
    /// <code>
    /// [&lt;Fact&gt;]
    /// let ``all routes are valid`` () =
    ///     let preconditions = [ Extractor.precondition authExtractor ]
    ///     let result = Route.validate&lt;Route, AppError&gt; preconditions
    ///     Assert.Equal(Ok (), result)
    /// </code>
    /// </example>
    let validate<'Route, 'E> (preconditions: PreconditionExtractor<'E> list) : Result<unit, string list> =
        let structureErrors =
            match validateStructure<'Route> () with
            | Ok() -> []
            | Error errors -> errors

        let uniquenessErrors =
            match validateUniqueness<'Route> () with
            | Ok() -> []
            | Error errors -> errors

        let preconditionErrors =
            match validatePreconditions<'Route, 'E> preconditions with
            | Ok() -> []
            | Error errors -> errors

        let allErrors = structureErrors @ uniquenessErrors @ preconditionErrors

        if allErrors.IsEmpty then Ok() else Error allErrors

    // =========================================================================
    // Public API - Hydration
    // =========================================================================

    /// Recursive hydration helper that works with obj types for reflection-based traversal.
    /// Returns Result<obj, 'E> where the obj is the hydrated route value.
    let rec private hydrateValue<'E>
        (preconditions: PreconditionExtractor<'E> list)
        (parsers: FieldParser list)
        (makeError: string -> 'E)
        (combineErrors: 'E list -> 'E)
        (leafCaseInfo: UnionCaseInfo)
        (value: obj)
        (ctx: HttpContext)
        : Result<obj, 'E> =

        let valueType = value.GetType()

        if not (FSharpType.IsUnion(valueType)) then
            Ok value
        else
            let caseInfo, fieldValues = FSharpValue.GetUnionFields(value, valueType)
            let fields = caseInfo.GetFields()

            if fields.Length = 0 then
                Ok value
            else
                let fieldResults =
                    fields
                    |> Array.mapi (fun i field ->
                        if isNestedRouteUnion field.PropertyType then
                            // Recursively hydrate nested route unions
                            match
                                hydrateValue
                                    preconditions
                                    parsers
                                    makeError
                                    combineErrors
                                    leafCaseInfo
                                    fieldValues.[i]
                                    ctx
                            with
                            | Ok hydratedNested -> FromExtraction(Ok hydratedNested)
                            | Error e -> FromPrecondition(Error e) // Use FromPrecondition to pass through 'E directly
                        elif
                            tryGetOptPreInfo field.PropertyType
                            |> Option.exists (fun innerType -> shouldSkipOptPrecondition leafCaseInfo innerType)
                        then
                            FromExtraction(Ok(createSkippedOptPreValue field.PropertyType))
                        else
                            match findPrecondition preconditions field.PropertyType with
                            | Some precondition -> FromPrecondition(precondition.Extract ctx)
                            | None -> FromExtraction(extractField parsers field.Name field.PropertyType ctx))

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
                    let values =
                        fieldResults
                        |> Array.map (function
                            | FromPrecondition(Ok v) -> v
                            | FromExtraction(Ok v) -> v
                            | FromPrecondition(Error _) -> failwith "unreachable"
                            | FromExtraction(Error _) -> failwith "unreachable")

                    let hydrated = FSharpValue.MakeUnion(caseInfo, values)
                    Ok hydrated

    /// Creates an extraction function that populates route fields from HTTP context.
    /// Internal - used by Route.endpoints. Hydration is recursive.
    let internal extractor<'Route, 'E>
        (preconditions: PreconditionExtractor<'E> list)
        (parsers: FieldParser list)
        (makeError: string -> 'E)
        (combineErrors: 'E list -> 'E)
        : 'Route -> Extractor<'Route, 'E> =

        let hydrateRoute route ctx =
            let leafCaseInfo = findLeafCaseInfo (box route)

            match hydrateValue preconditions parsers makeError combineErrors leafCaseInfo (box route) ctx with
            | Ok hydratedObj -> Ok(hydratedObj :?> 'Route)
            | Error e -> Error e

        fun route -> hydrateRoute route

    // =========================================================================
    // Public API - Falco Integration
    // =========================================================================

    /// <summary>Generates Falco endpoints with automatic route extraction.</summary>
    /// <typeparam name="TRoute">The route union type.</typeparam>
    /// <typeparam name="E">The error type for extraction failures.</typeparam>
    /// <param name="config">Configuration for extraction (preconditions, parsers, error handling).</param>
    /// <param name="routeHandler">A function that takes a hydrated route value and returns an HTTP handler.</param>
    /// <returns>A list of Falco <c>HttpEndpoint</c> values ready for use with <c>app.UseFalco</c>.</returns>
    /// <remarks>
    /// <para>This is the main entry point for Falco.UnionRoutes. It:</para>
    /// <list type="bullet">
    ///   <item><description>Validates route structure at startup</description></item>
    ///   <item><description>Extracts route/query parameters automatically</description></item>
    ///   <item><description>Runs precondition extractors (auth, validation)</description></item>
    ///   <item><description>Hydrates nested routes recursively</description></item>
    /// </list>
    /// </remarks>
    /// <example>
    /// <code>
    /// let config: EndpointConfig&lt;AppError&gt; = {
    ///     Preconditions = [ Extractor.precondition&lt;UserId, _&gt; requireAuth ]
    ///     Parsers = []
    ///     MakeError = fun msg -> BadRequest msg
    ///     CombineErrors = fun errors -> errors |> List.head
    ///     ToErrorResponse = fun e -> Response.withStatusCode 400 >> Response.ofPlainText (string e)
    /// }
    ///
    /// let handleRoute route =
    ///     match route with
    ///     | Home -> Response.ofPlainText "home"
    ///     | Posts p -> handlePost p
    ///
    /// let endpoints = Route.endpoints config handleRoute
    /// app.UseFalco(endpoints) |> ignore
    /// </code>
    /// </example>
    let endpoints<'TRoute, 'E> (config: EndpointConfig<'E>) (routeHandler: 'TRoute -> HttpHandler) : HttpEndpoint list =
        let structureErrors =
            match validateStructure<'TRoute> () with
            | Ok() -> []
            | Error errors -> errors

        let uniquenessErrors =
            match validateUniqueness<'TRoute> () with
            | Ok() -> []
            | Error errors -> errors

        let allErrors = structureErrors @ uniquenessErrors

        if not allErrors.IsEmpty then
            let errorMsg = allErrors |> String.concat "\n  - "
            failwith $"Route validation failed:\n  - {errorMsg}"

        let hydrate =
            extractor<'TRoute, 'E> config.Preconditions config.Parsers config.MakeError config.CombineErrors

        allRoutes<'TRoute> ()
        |> List.map (fun route ->
            let routeInfo = info route
            let handler = Extraction.run config.ToErrorResponse (hydrate route) routeHandler
            let sortKey = parsePathSegments routeInfo.Path
            (sortKey, routeInfo.Path, toFalcoMethod routeInfo.Method routeInfo.Path handler))
        |> List.sortBy (fun (sortKey, path, _) ->
            (sortKey
             |> List.map (function
                 | Literal _ -> 0
                 | Parameter -> 1),
             path))
        |> List.map (fun (_, _, endpoint) -> endpoint)
