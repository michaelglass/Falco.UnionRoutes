namespace Falco.UnionRoutes

open System
open System.Text.RegularExpressions
open Falco
open Falco.Routing
open FSharp.Reflection

/// Reflection utilities for extracting route information from discriminated unions
module RouteReflection =

    // Precompiled regex for better performance during route enumeration
    let private kebabCaseRegex =
        Regex(@"([a-z])([A-Z])|([A-Z]+)([A-Z][a-z])", RegexOptions.Compiled)

    /// Convert PascalCase to kebab-case (e.g., "DigestView" -> "digest-view", "HTMLParser" -> "html-parser")
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

    /// Convert RouteMethod enum to HttpMethod DU
    let toHttpMethod (rm: RouteMethod) : HttpMethod =
        match rm with
        | RouteMethod.Get -> HttpMethod.Get
        | RouteMethod.Post -> HttpMethod.Post
        | RouteMethod.Put -> HttpMethod.Put
        | RouteMethod.Delete -> HttpMethod.Delete
        | RouteMethod.Patch -> HttpMethod.Patch
        | RouteMethod.Any -> HttpMethod.Any
        | unknown -> failwith $"Unknown RouteMethod: {int unknown}"

    /// Get RouteAttribute from a union case, if present
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
    /// Excludes: strings, options, Pre<'T>, Query<'T>, single-case wrappers
    let private isNestedRouteUnion (t: Type) =
        FSharpType.IsUnion(t)
        && t <> typeof<string>
        && not (t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>)
        && not (isPreconditionType t)
        && not (isQueryType t)
        && not (isSingleCaseWrapper t)

    /// Check if a field should be excluded from route path (nested route unions, preconditions, query params)
    let private isNonRouteField (f: Reflection.PropertyInfo) =
        isNestedRouteUnion f.PropertyType
        || isPreconditionType f.PropertyType
        || isQueryType f.PropertyType
        || isOptionalQueryType f.PropertyType

    /// Infer path segment from case fields (e.g., "of id: Guid" -> "{id}")
    /// Excludes: nested route unions, Pre<'T> (preconditions), Query<'T> (query params)
    let private inferPathFromFields (case: UnionCaseInfo) : string option =
        let fields = case.GetFields()

        if fields.Length = 0 then
            None
        elif fields.Length = 1 && isNestedRouteUnion fields.[0].PropertyType then
            None // Nested route union, not a path parameter
        else
            // Filter out non-route fields (nested unions, preconditions, query params)
            let pathFields = fields |> Array.filter (fun f -> not (isNonRouteField f))

            if pathFields.Length = 0 then
                None
            else
                pathFields
                |> Array.map (fun f -> "{" + f.Name + "}")
                |> String.concat "/"
                |> Some

    /// Check if a case has any route path fields (i.e., typed arguments like "of id: Guid")
    /// Excludes: nested route unions, Pre<'T> (preconditions), Query<'T> (query params)
    let private hasRoutePathFields (case: UnionCaseInfo) : bool =
        case.GetFields() |> Array.exists (fun f -> not (isNonRouteField f))

    /// Get path segment from a case (uses attribute Path if set, otherwise infers from fields or case name)
    /// Special cases for empty path: "Root", "List", "Create"
    let private getPathSegment (case: UnionCaseInfo) : string =
        match getRouteAttr case |> Option.bind getAttrPath with
        | Some path -> path
        | None ->
            // First try to infer from fields
            match inferPathFromFields case with
            | Some path -> path
            | None ->
                // No inferable fields, check for special case names
                match case.Name with
                | "Root"
                | "List"
                | "Create" -> ""
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

    /// Full route metadata extracted via reflection
    type RouteInfo = { Method: HttpMethod; Path: string }

    /// Get full route metadata for a route value using reflection.
    /// Returns None if the route has no RouteAttribute.
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

    /// Get full route metadata for a route value using reflection.
    /// Throws if the route has no RouteAttribute.
    let routeInfo (route: 'T) : RouteInfo =
        match tryRouteInfo route with
        | Some info -> info
        | None ->
            let case, _ = FSharpValue.GetUnionFields(route, typeof<'T>)
            failwithf "Route case '%s' is missing [<Route(...)>] attribute" case.Name

    /// Get (HttpMethod, pathPattern) tuple for a route value.
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

    /// Generate a concrete URL path from a route value by substituting actual field values.
    /// Example: link (Posts (Detail (Guid.Parse "abc..."))) returns "/posts/abc..."
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

    /// Enumerate all route cases for a route union type.
    /// Parameterized routes are created with default values (Guid.Empty, "", 0, etc.)
    let allRoutes<'TRoute> () : 'TRoute list =
        enumerateUnionValues typeof<'TRoute> |> List.map (fun o -> o :?> 'TRoute)

    // =========================================================================
    // Falco integration
    // =========================================================================

    /// Convert HttpMethod to Falco's route function (get, post, put, delete, patch, any)
    let toFalcoMethod (method: HttpMethod) =
        match method with
        | HttpMethod.Get -> get
        | HttpMethod.Post -> post
        | HttpMethod.Put -> put
        | HttpMethod.Delete -> delete
        | HttpMethod.Patch -> patch
        | HttpMethod.Any -> any

    /// Generate Falco endpoints from a route handler function.
    /// Enumerates all routes of the given type and maps each to an HttpEndpoint.
    let endpoints (routeHandler: 'TRoute -> HttpHandler) : HttpEndpoint list =
        allRoutes<'TRoute> ()
        |> List.map (fun route ->
            let info = routeInfo route
            let handler = routeHandler route
            toFalcoMethod info.Method info.Path handler)
