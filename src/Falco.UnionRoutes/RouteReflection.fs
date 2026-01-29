namespace Falco.UnionRoutes

open System
open System.Text.RegularExpressions
open FSharp.Reflection

/// Reflection utilities for extracting route information from discriminated unions
module RouteReflection =

    /// Convert PascalCase to kebab-case (e.g., "DigestView" -> "digest-view")
    let toKebabCase (s: string) =
        Regex.Replace(s, "([a-z])([A-Z])", "$1-$2").ToLowerInvariant()

    /// Convert RouteMethod enum to HttpMethod DU
    let toHttpMethod (rm: RouteMethod) : HttpMethod =
        match rm with
        | RouteMethod.Get -> HttpMethod.Get
        | RouteMethod.Post -> HttpMethod.Post
        | RouteMethod.Put -> HttpMethod.Put
        | RouteMethod.Delete -> HttpMethod.Delete
        | RouteMethod.Patch -> HttpMethod.Patch
        | _ -> HttpMethod.Get

    /// Get RouteAttribute from a union case, if present
    let getRouteAttr (case: UnionCaseInfo) : RouteAttribute option =
        case.GetCustomAttributes(typeof<RouteAttribute>)
        |> Array.tryHead
        |> Option.map (fun a -> a :?> RouteAttribute)

    /// Get path segment from a case (uses attribute Path if set, otherwise kebab-case name)
    let private getPathSegment (case: UnionCaseInfo) : string =
        match getRouteAttr case with
        | Some attr when not (isNull attr.Path) -> attr.Path
        | _ -> toKebabCase case.Name

    /// Get HTTP method from a case's RouteAttribute
    let private getMethod (case: UnionCaseInfo) : HttpMethod option =
        getRouteAttr case |> Option.map (fun attr -> toHttpMethod attr.Method)

    /// Check if a type is a nested route union (for hierarchy traversal)
    let private isNestedRouteUnion (t: Type) =
        FSharpType.IsUnion(t)
        && t <> typeof<string>
        && not (t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>)

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
