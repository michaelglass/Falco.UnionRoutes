namespace Falco.UnionRoutes

open System
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection

/// <summary>Configuration for OpenAPI spec generation.</summary>
type OpenApiConfig =
    {
        /// <summary>The title of the API.</summary>
        Title: string
        /// <summary>The version of the API (e.g., "1.0.0").</summary>
        Version: string
        /// <summary>Optional description of the API.</summary>
        Description: string option
    }

/// <summary>Generates OpenAPI 3.0 JSON specifications from route union types.</summary>
/// <remarks>
/// <para>Uses reflection to enumerate all routes and build a complete OpenAPI 3.0 spec.
/// Route paths, HTTP methods, path/query parameters, request bodies, and response types
/// are all derived from the route union structure.</para>
/// </remarks>
/// <example>
/// <code>
/// let spec = Spec.generate&lt;Route&gt; { Title = "My API"; Version = "1.0.0"; Description = None }
/// </code>
/// </example>
[<RequireQualifiedAccess>]
module Spec =

    /// Strip route constraints from path templates for OpenAPI (e.g., {id:guid} -> {id})
    let private stripConstraints (path: string) : string =
        Regex.Replace(path, @"\{([^:}]+)(:[^}]*)?\}", "{${1}}")

    /// Map an F# type to an OpenAPI JSON Schema object.
    /// Returns (schema, schemasToAdd) where schemasToAdd are record types for components/schemas.
    let rec private typeToSchema (schemas: JsonObject) (t: Type) : JsonObject =
        if t = typeof<int> then
            let o = JsonObject()
            o["type"] <- "integer"
            o["format"] <- "int32"
            o
        elif t = typeof<int64> then
            let o = JsonObject()
            o["type"] <- "integer"
            o["format"] <- "int64"
            o
        elif t = typeof<bool> then
            let o = JsonObject()
            o["type"] <- "boolean"
            o
        elif t = typeof<string> then
            let o = JsonObject()
            o["type"] <- "string"
            o
        elif t = typeof<Guid> then
            let o = JsonObject()
            o["type"] <- "string"
            o["format"] <- "uuid"
            o
        elif t = typeof<float> || t = typeof<double> then
            let o = JsonObject()
            o["type"] <- "number"
            o["format"] <- "double"
            o
        elif t = typeof<float32> || t = typeof<single> then
            let o = JsonObject()
            o["type"] <- "number"
            o["format"] <- "float"
            o
        elif t = typeof<decimal> then
            let o = JsonObject()
            o["type"] <- "number"
            o["format"] <- "decimal"
            o
        elif Route.isSingleCaseWrapper t then
            let innerType = FSharpType.GetUnionCases(t).[0].GetFields().[0].PropertyType
            typeToSchema schemas innerType
        elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
            let innerType = t.GetGenericArguments().[0]
            let inner = typeToSchema schemas innerType
            inner["nullable"] <- true
            inner
        elif t.IsGenericType && (t.GetGenericTypeDefinition() = typedefof<list<_>>) then
            let innerType = t.GetGenericArguments().[0]
            let o = JsonObject()
            o["type"] <- "array"
            o["items"] <- typeToSchema schemas innerType
            o
        elif t.IsArray then
            let innerType = t.GetElementType()
            let o = JsonObject()
            o["type"] <- "array"
            o["items"] <- typeToSchema schemas innerType
            o
        elif FSharpType.IsRecord t then
            let name = t.Name

            if isNull (schemas[name]) then
                let props = JsonObject()
                let required = JsonArray()

                for field in FSharpType.GetRecordFields t do
                    let fieldType = field.PropertyType

                    let isOption =
                        fieldType.IsGenericType
                        && fieldType.GetGenericTypeDefinition() = typedefof<option<_>>

                    let actualType =
                        if isOption then
                            fieldType.GetGenericArguments().[0]
                        else
                            fieldType

                    props[field.Name] <- typeToSchema schemas actualType

                    if not isOption then
                        required.Add(field.Name)

                let schemaDef = JsonObject()
                schemaDef["type"] <- "object"
                schemaDef["properties"] <- props

                if required.Count > 0 then
                    schemaDef["required"] <- required

                schemas[name] <- schemaDef

            let ref = JsonObject()
            ref["$ref"] <- $"#/components/schemas/{name}"
            ref
        else
            let o = JsonObject()
            o["type"] <- "object"
            o

    /// Classify fields from a route union case hierarchy.
    [<NoComparison; NoEquality>]
    type private FieldClassification =
        | PathParam of name: string * fieldType: Type
        | QueryParam of name: string * innerType: Type * required: bool
        | JsonBodyField of innerType: Type
        | FormBodyField of innerType: Type
        | ResponseType of innerType: Type
        | Precondition
        | NestedRoute

    /// Classify all fields for a union case
    let private classifyFields (case: UnionCaseInfo) : FieldClassification list =
        case.GetFields()
        |> Array.toList
        |> List.map (fun f ->
            let t = f.PropertyType

            if Route.isPreconditionType t || Route.isOptionalPreconditionType t then
                Precondition
            elif Route.isReturnsType t then
                ResponseType(t.GetGenericArguments().[0])
            elif Route.isJsonBodyType t then
                JsonBodyField(t.GetGenericArguments().[0])
            elif Route.isFormBodyType t then
                FormBodyField(t.GetGenericArguments().[0])
            elif Route.isNestedRouteUnion t then
                NestedRoute
            elif Route.isOptionalQueryType t then
                let queryType = t.GetGenericArguments().[0]
                let innerType = queryType.GetGenericArguments().[0]
                QueryParam(f.Name, innerType, false)
            elif Route.isQueryType t then
                let innerType = t.GetGenericArguments().[0]
                QueryParam(f.Name, innerType, true)
            else
                PathParam(f.Name, t))

    /// Recursively collect all field classifications from a route value
    let rec private collectAllFields (value: obj) : FieldClassification list =
        let valueType = value.GetType()
        let case, fieldValues = FSharpValue.GetUnionFields(value, valueType)
        let fieldInfos = case.GetFields()
        let classifications = classifyFields case

        let nestedFields =
            fieldInfos
            |> Array.indexed
            |> Array.tryFind (fun (_, f) -> Route.isNestedRouteUnion f.PropertyType)
            |> Option.map (fun (idx, _) -> collectAllFields fieldValues.[idx])
            |> Option.defaultValue []

        let ownFields =
            classifications
            |> List.filter (fun c ->
                match c with
                | NestedRoute -> false
                | _ -> true)

        ownFields @ nestedFields

    /// Convert HttpMethod to lowercase OpenAPI method string
    let private methodToString (method: HttpMethod) : string =
        match method with
        | HttpMethod.Get -> "get"
        | HttpMethod.Post -> "post"
        | HttpMethod.Put -> "put"
        | HttpMethod.Delete -> "delete"
        | HttpMethod.Patch -> "patch"
        | HttpMethod.Any -> "get"

    /// <summary>Generates an OpenAPI 3.0 JSON specification from a route union type.</summary>
    /// <typeparam name="TRoute">The route union type to generate the spec from.</typeparam>
    /// <param name="config">OpenAPI metadata (title, version, description).</param>
    /// <returns>A JSON string containing the OpenAPI 3.0 specification.</returns>
    /// <example>
    /// <code>
    /// let spec = Spec.generate&lt;Route&gt; { Title = "My API"; Version = "1.0.0"; Description = None }
    /// printfn "%s" spec
    /// </code>
    /// </example>
    let generate<'TRoute> (config: OpenApiConfig) : string =
        let root = JsonObject()
        root["openapi"] <- "3.0.0"

        let info = JsonObject()
        info["title"] <- config.Title
        info["version"] <- config.Version

        match config.Description with
        | Some desc -> info["description"] <- desc
        | None -> ()

        root["info"] <- info

        let paths = JsonObject()
        let schemas = JsonObject()

        let allRoutes = Route.allRoutes<'TRoute> ()

        for route in allRoutes do
            let routeInfo = Route.info route
            let openApiPath = stripConstraints routeInfo.Path
            let methodStr = methodToString routeInfo.Method
            let fields = collectAllFields (box route)

            let operation = JsonObject()

            // Path parameters
            let parameters = JsonArray()

            for field in fields do
                match field with
                | PathParam(name, fieldType) ->
                    let param = JsonObject()
                    param["name"] <- name
                    param["in"] <- "path"
                    param["required"] <- true
                    param["schema"] <- typeToSchema schemas fieldType
                    parameters.Add(param)
                | QueryParam(name, innerType, required) ->
                    let param = JsonObject()
                    param["name"] <- name
                    param["in"] <- "query"
                    param["required"] <- required
                    param["schema"] <- typeToSchema schemas innerType
                    parameters.Add(param)
                | _ -> ()

            if parameters.Count > 0 then
                operation["parameters"] <- parameters

            // Request body
            let bodyField =
                fields
                |> List.tryFind (fun f ->
                    match f with
                    | JsonBodyField _
                    | FormBodyField _ -> true
                    | _ -> false)

            match bodyField with
            | Some(JsonBodyField innerType) ->
                let requestBody = JsonObject()
                requestBody["required"] <- true
                let content = JsonObject()
                let mediaType = JsonObject()
                mediaType["schema"] <- typeToSchema schemas innerType
                content["application/json"] <- mediaType
                requestBody["content"] <- content
                operation["requestBody"] <- requestBody
            | Some(FormBodyField innerType) ->
                let requestBody = JsonObject()
                requestBody["required"] <- true
                let content = JsonObject()
                let mediaType = JsonObject()
                mediaType["schema"] <- typeToSchema schemas innerType
                content["application/x-www-form-urlencoded"] <- mediaType
                requestBody["content"] <- content
                operation["requestBody"] <- requestBody
            | _ -> ()

            // Responses
            let responses = JsonObject()

            let responseType =
                fields
                |> List.tryFind (fun f ->
                    match f with
                    | ResponseType _ -> true
                    | _ -> false)

            match responseType with
            | Some(ResponseType innerType) ->
                let okResponse = JsonObject()
                okResponse["description"] <- "Successful response"
                let content = JsonObject()
                let mediaType = JsonObject()
                mediaType["schema"] <- typeToSchema schemas innerType
                content["application/json"] <- mediaType
                okResponse["content"] <- content
                responses["200"] <- okResponse
            | _ ->
                let okResponse = JsonObject()
                okResponse["description"] <- "Successful response"
                responses["200"] <- okResponse

            operation["responses"] <- responses

            // Add to paths
            let pathItem =
                match paths[openApiPath] with
                | null ->
                    let p = JsonObject()
                    paths[openApiPath] <- p
                    p
                | existing -> existing :?> JsonObject

            pathItem[methodStr] <- operation

        root["paths"] <- paths

        if schemas.Count > 0 then
            let components = JsonObject()
            components["schemas"] <- schemas
            root["components"] <- components

        let options = JsonSerializerOptions(WriteIndented = true)
        root.ToJsonString(options)
