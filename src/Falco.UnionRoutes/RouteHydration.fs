namespace Falco.UnionRoutes

open System
open Falco
open Microsoft.AspNetCore.Http
open Microsoft.FSharp.Reflection

/// A custom type extractor for route hydration.
/// Takes field name, field type, and HTTP context.
/// Returns Some (Ok value) if extraction succeeds, Some (Error message) if it fails,
/// or None to defer to other extractors.
type TypeExtractor = string -> Type -> HttpContext -> Result<obj, string> option

/// Wrapper type to indicate a field should be extracted from query string instead of route params.
/// Example: `| Search of query: Query<string>` extracts from `?query=...`
type Query<'T> = Query of 'T

/// Automatic route hydration based on route type structure.
///
/// Examines each field in a route case and:
/// - If the field type matches the auth type, uses the auth pipeline
/// - Custom extractors are tried next (if provided)
/// - If the field is Guid (or a wrapper like PostId), extracts from route params
/// - If the field is string, extracts from route params using field name
/// - If the field is int, extracts from route params using field name
/// - If the field is Query<'T>, extracts from query string (required)
/// - If the field is Query<'T> option, extracts from query string (optional)
///
/// Wrapper types (single-case DUs like `type PostId = PostId of Guid`) are
/// automatically detected and unwrapped for extraction, then re-wrapped.
///
/// Example:
/// ```fsharp
/// type PostId = PostId of Guid
/// type PostRoute =
///     | List of page: Query<int> option       // optional query param
///     | Detail of PostId                      // extracts Guid, wraps in PostId
///     | Create of UserId                      // auth field
///     | Delete of UserId * PostId
///     | Search of query: Query<string>        // required query param
///
/// let hydrate = RouteHydration.create<PostRoute, UserId, AppError> requireAuth badRequestError
/// let handler route = Pipeline.run toError (hydrate route) handlePost
/// ```
[<RequireQualifiedAccess>]
module RouteHydration =

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

    /// Extracts a primitive value (Guid, string, int) from route parameters.
    /// Returns Ok value, Error for invalid, or None if missing (for optional handling).
    let private tryExtractPrimitiveFromRoute
        (fieldName: string)
        (primitiveType: Type)
        (ctx: HttpContext)
        : Result<obj, string> option =

        let route = Request.getRoute ctx

        if primitiveType = typeof<Guid> then
            let value = route.GetString fieldName

            if String.IsNullOrEmpty(value) then
                None // Missing
            else
                match Guid.TryParse(value) with
                | true, guid -> Some(Ok(box guid))
                | false, _ -> Some(Error $"Invalid GUID route parameter: {fieldName}")
        elif primitiveType = typeof<string> then
            let value = route.GetString fieldName

            if String.IsNullOrEmpty(value) then
                None // Missing
            else
                Some(Ok(box value))
        elif primitiveType = typeof<int> then
            let value = route.GetString fieldName

            if String.IsNullOrEmpty(value) then
                None // Missing
            else
                match Int32.TryParse(value) with
                | true, i -> Some(Ok(box i))
                | false, _ -> Some(Error $"Invalid integer route parameter: {fieldName}")
        else
            Some(Error $"Unsupported primitive type: {primitiveType.Name}")

    /// Extracts a primitive value from query string.
    /// Returns Ok value, Error for invalid, or None if missing (for optional handling).
    let private tryExtractPrimitiveFromQuery
        (fieldName: string)
        (primitiveType: Type)
        (ctx: HttpContext)
        : Result<obj, string> option =

        let query = Request.getQuery ctx

        if primitiveType = typeof<Guid> then
            let value = query.GetString fieldName

            if String.IsNullOrEmpty(value) then
                None
            else
                match Guid.TryParse(value) with
                | true, guid -> Some(Ok(box guid))
                | false, _ -> Some(Error $"Invalid GUID query parameter: {fieldName}")
        elif primitiveType = typeof<string> then
            let value = query.GetString fieldName

            if String.IsNullOrEmpty(value) then
                None
            else
                Some(Ok(box value))
        elif primitiveType = typeof<int> then
            let value = query.GetString fieldName

            if String.IsNullOrEmpty(value) then
                None
            else
                match Int32.TryParse(value) with
                | true, i -> Some(Ok(box i))
                | false, _ -> Some(Error $"Invalid integer query parameter: {fieldName}")
        else
            Some(Error $"Unsupported primitive type for query: {primitiveType.Name}")

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
                match tryExtractPrimitiveFromQuery fieldName innerType ctx with
                | Some(Ok v) ->
                    let queryCase = FSharpType.GetUnionCases(fieldType).[0]
                    Ok(FSharpValue.MakeUnion(queryCase, [| v |]))
                | Some(Error msg) -> Error msg
                | None -> Error $"Missing query parameter: {fieldName}"
            | None ->
                // Check for primitives
                if
                    fieldType = typeof<Guid>
                    || fieldType = typeof<string>
                    || fieldType = typeof<int>
                then
                    tryExtractPrimitiveFromRoute fieldName fieldType ctx
                    |> Option.defaultValue (Error $"Missing route parameter: {fieldName}")
                else
                    // Check if it's a wrapper type (single-case DU like PostId)
                    match tryGetWrapperInfo fieldType with
                    | Some(innerType, wrapFn) ->
                        tryExtractPrimitiveFromRoute fieldName innerType ctx
                        |> Option.defaultValue (Error $"Missing route parameter: {fieldName}")
                        |> Result.map wrapFn
                    | None ->
                        failwith
                            $"RouteHydration: Don't know how to extract field '{fieldName}' of type {fieldType.Name}"

    /// Extracts a non-auth field value from route parameters based on type.
    /// Handles option types, Query types, custom extractors, primitives, and wrapper types.
    let private extractNonAuthField
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
                match tryExtractPrimitiveFromQuery fieldName queryInnerType ctx with
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

    /// Creates a hydration function for a route type with custom extractors.
    /// All errors are accumulated and combined using combineErrors.
    ///
    /// The hydration function examines the route case and extracts values for each field:
    /// - Fields matching 'Auth type use the provided auth pipeline (errors preserve type)
    /// - Custom extractors are tried next (in order)
    /// - Guid fields are extracted from route parameters by field name
    /// - String fields are extracted from route parameters by field name
    /// - Int fields are extracted from route parameters by field name
    /// - Single-case DU wrappers (like PostId of Guid) are auto-detected
    /// - Query<'T> extracts from query string
    /// - Query<'T> option for optional query params
    ///
    /// Error handling:
    /// - Extraction errors (strings) are converted to 'Error via makeError
    /// - All errors (auth and extraction) are accumulated
    /// - Combined into final error via combineErrors
    let createWith<'Route, 'Auth, 'Error>
        (extractors: TypeExtractor list)
        (authPipeline: Pipeline<'Auth, 'Error>)
        (makeError: string -> 'Error)
        (combineErrors: 'Error list -> 'Error)
        : 'Route -> Pipeline<'Route, 'Error> =

        let routeType = typeof<'Route>
        let authType = typeof<'Auth>

        fun (route: 'Route) ->
            fun (ctx: HttpContext) ->
                // Get the union case and current field values
                let caseInfo, _ = FSharpValue.GetUnionFields(route, routeType)
                let fields = caseInfo.GetFields()

                if fields.Length = 0 then
                    // No fields to extract - just return the route as-is
                    Ok route
                else
                    // Check if any field requires auth
                    let hasAuthField = fields |> Array.exists (fun f -> f.PropertyType = authType)

                    // If auth is needed, run auth pipeline first
                    let authResult =
                        if hasAuthField then
                            authPipeline ctx |> Result.map box
                        else
                            Ok(box ()) // Placeholder, won't be used

                    // Extract all non-auth fields (regardless of auth result, for error accumulation)
                    let extractedNonAuthValues =
                        fields
                        |> Array.map (fun field ->
                            if field.PropertyType = authType then
                                None // Will be filled from authResult
                            else
                                Some(extractNonAuthField extractors field.Name field.PropertyType ctx))

                    // Collect extraction errors and convert to 'Error
                    let extractionErrors =
                        extractedNonAuthValues
                        |> Array.choose (function
                            | Some(Error e) -> Some(makeError e)
                            | _ -> None)
                        |> Array.toList

                    // Collect auth error if any
                    let authErrors =
                        match authResult with
                        | Error e -> [ e ]
                        | Ok _ -> []

                    // Combine all errors
                    let allErrors = authErrors @ extractionErrors

                    if allErrors.Length > 0 then
                        Error(combineErrors allErrors)
                    else
                        // All succeeded - construct the hydrated route
                        let authValue =
                            match authResult with
                            | Ok v -> v
                            | Error _ -> failwith "Impossible: checked for errors above"

                        let values =
                            (fields, extractedNonAuthValues)
                            ||> Array.map2 (fun field extracted ->
                                if field.PropertyType = authType then
                                    authValue
                                else
                                    match extracted with
                                    | Some(Ok v) -> v
                                    | _ -> failwith "Impossible: checked for errors above")

                        let hydrated = FSharpValue.MakeUnion(caseInfo, values) :?> 'Route
                        Ok hydrated

    /// Creates a hydration function for a route type.
    /// All errors are accumulated and combined using combineErrors.
    let create<'Route, 'Auth, 'Error>
        (authPipeline: Pipeline<'Auth, 'Error>)
        (makeError: string -> 'Error)
        (combineErrors: 'Error list -> 'Error)
        : 'Route -> Pipeline<'Route, 'Error> =
        createWith [] authPipeline makeError combineErrors

    /// Creates a hydration function for routes that don't need authentication.
    /// Only extracts route parameters (Guid, string, int, wrapper types, and Query<'T>).
    let createNoAuth<'Route, 'Error>
        (makeError: string -> 'Error)
        (combineErrors: 'Error list -> 'Error)
        : 'Route -> Pipeline<'Route, 'Error> =
        createWith<'Route, unit, 'Error> [] (fun _ -> Error(makeError "No auth configured")) makeError combineErrors

    /// Creates a hydration function with custom extractors but no authentication.
    let createNoAuthWith<'Route, 'Error>
        (extractors: TypeExtractor list)
        (makeError: string -> 'Error)
        (combineErrors: 'Error list -> 'Error)
        : 'Route -> Pipeline<'Route, 'Error> =
        createWith<'Route, unit, 'Error>
            extractors
            (fun _ -> Error(makeError "No auth configured"))
            makeError
            combineErrors
