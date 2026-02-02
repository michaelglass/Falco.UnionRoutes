namespace Falco.UnionRoutes

open System
open Falco
open Microsoft.AspNetCore.Http
open Microsoft.FSharp.Reflection

/// Automatic route hydration based on route type structure.
///
/// Examines each field in a route case and:
/// - If the field type matches the auth type, uses the auth pipeline
/// - If the field is Guid, extracts from route params using field name
/// - If the field is string, extracts from route params using field name
/// - If the field is int, extracts from route params using field name
///
/// Example:
/// ```fsharp
/// type PostRoute =
///     | List
///     | Detail of id: Guid
///     | Create of UserId           // auth field
///     | Delete of UserId * id: Guid
///
/// let hydrate = RouteHydration.create<PostRoute, UserId, AppError> requireAuth badRequestError
/// let handler route = Pipeline.run toError (hydrate route) handlePost
/// ```
[<RequireQualifiedAccess>]
module RouteHydration =

    /// Extracts a value from route parameters based on type
    let private extractField
        (fieldName: string)
        (fieldType: Type)
        (authType: Type)
        (authPipeline: HttpContext -> Result<obj, 'Error>)
        (makeError: string -> 'Error)
        (ctx: HttpContext)
        : Result<obj, 'Error> =

        if fieldType = authType then
            authPipeline ctx
        elif fieldType = typeof<Guid> then
            match tryGetRouteGuid ctx fieldName with
            | Some guid -> Ok(box guid)
            | None -> Error(makeError $"Invalid or missing route parameter: {fieldName}")
        elif fieldType = typeof<string> then
            let route = Request.getRoute ctx
            let value = route.GetString fieldName

            if String.IsNullOrEmpty(value) then
                Error(makeError $"Missing route parameter: {fieldName}")
            else
                Ok(box value)
        elif fieldType = typeof<int> then
            let route = Request.getRoute ctx
            let value = route.GetString fieldName

            match Int32.TryParse(value) with
            | true, i -> Ok(box i)
            | false, _ -> Error(makeError $"Invalid integer route parameter: {fieldName}")
        else
            failwith $"RouteHydration: Don't know how to extract field '{fieldName}' of type {fieldType.Name}"

    /// Creates a hydration function for a route type.
    ///
    /// The hydration function examines the route case and extracts values for each field:
    /// - Fields matching 'Auth type use the provided auth pipeline
    /// - Guid fields are extracted from route parameters by field name
    /// - String fields are extracted from route parameters by field name
    /// - Int fields are extracted from route parameters by field name
    let create<'Route, 'Auth, 'Error>
        (authPipeline: Pipeline<'Auth, 'Error>)
        (makeError: string -> 'Error)
        : 'Route -> Pipeline<'Route, 'Error> =

        let routeType = typeof<'Route>
        let authType = typeof<'Auth>

        // Box the auth pipeline to work with obj
        let boxedAuthPipeline: HttpContext -> Result<obj, 'Error> =
            fun ctx -> authPipeline ctx |> Result.map box

        fun (route: 'Route) ->
            fun (ctx: HttpContext) ->
                // Get the union case and current field values
                let caseInfo, _ = FSharpValue.GetUnionFields(route, routeType)
                let fields = caseInfo.GetFields()

                if fields.Length = 0 then
                    // No fields to extract - just return the route as-is
                    Ok route
                else
                    // Extract each field value
                    let extractedValues =
                        fields
                        |> Array.map (fun field ->
                            extractField field.Name field.PropertyType authType boxedAuthPipeline makeError ctx)

                    // Check if any extraction failed
                    let firstError =
                        extractedValues
                        |> Array.tryPick (function
                            | Error e -> Some e
                            | Ok _ -> None)

                    match firstError with
                    | Some e -> Error e
                    | None ->
                        // All succeeded - construct the hydrated route
                        let values =
                            extractedValues
                            |> Array.map (function
                                | Ok v -> v
                                | Error _ -> failwith "Impossible: checked for errors above")

                        let hydrated = FSharpValue.MakeUnion(caseInfo, values) :?> 'Route
                        Ok hydrated
