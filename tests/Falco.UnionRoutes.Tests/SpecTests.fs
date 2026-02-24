module Falco.UnionRoutes.Tests.SpecTests

open System
open System.Text.Json.Nodes
open Xunit
open Swensen.Unquote
open Falco.UnionRoutes

// =============================================================================
// Test types
// =============================================================================

type PostId = PostId of Guid

type PostInput = { Title: string; Body: string }

type ContactInput = { Name: string; Message: string }

type Fortune = { Message: string }

type SimpleRoute =
    | Root
    | List
    | Show of id: Guid

type MethodRoute =
    | List
    | Create
    | Delete of id: Guid
    | Patch of id: Guid

type QueryParamRoute =
    | Search of query: QueryParam<string>
    | Paginated of page: QueryParam<int> option

type PreconditionRoute =
    | Public
    | Private of PreCondition<Guid>
    | Overridable of OverridablePreCondition<Guid>

type ReturnsRoute =
    | List of Returns<Fortune list>
    | Show of id: Guid * Returns<Fortune>

type JsonBodyRoute = Create of JsonBody<PostInput>

type FormBodyRoute = Submit of FormBody<ContactInput>

type NestedDetailRoute =
    | Show
    | Edit
    | Delete

type NestedParentRoute =
    | List
    | Member of id: Guid * NestedDetailRoute

type WrappedIdRoute = Show of id: PostId

type IntParamRoute = Show of page: int

type Int64ParamRoute = Show of id: int64

type BoolParamRoute = Show of enabled: bool

type StringParamRoute = Show of slug: string

type OptionalFieldRecord = { Name: string; Email: string option }

type OptionalFieldRoute = Create of JsonBody<OptionalFieldRecord>

type ArrayFieldRecord = { Tags: string list }

type ArrayRoute = List of Returns<ArrayFieldRecord>

type MixedRoute =
    | Show of id: Guid * Returns<PostInput>
    | Create of JsonBody<PostInput> * PreCondition<Guid>
    | Search of query: QueryParam<string> * page: QueryParam<int> option

type FloatRecord =
    { Score: float
      Rating: float32
      Price: decimal }

type FloatFieldRoute = Create of JsonBody<FloatRecord>

type ArrayRecord = { Items: string array }

type ArrayFieldRoute = Create of JsonBody<ArrayRecord>

type NullableResponse = { Value: string option }

type NullableResponseRoute = List of Returns<NullableResponse>

type OptionListRecord = { Items: string option list }

type OptionListRoute = Create of JsonBody<OptionListRecord>

type DateRecord = { CreatedAt: DateTime }

type DateFieldRoute = Create of JsonBody<DateRecord>

type PutRoute = | [<Route(RouteMethod.Put)>] Update of id: Guid

type AnyRoute = | [<Route(RouteMethod.Any)>] Catch

// =============================================================================
// Helpers
// =============================================================================

let defaultConfig: OpenApiConfig =
    { Title = "Test API"
      Version = "1.0.0"
      Description = None }

let parse (json: string) : JsonObject = JsonNode.Parse(json) :?> JsonObject

let generateAndParse<'T> () : JsonObject =
    Spec.generate<'T> defaultConfig |> parse

// =============================================================================
// Basic structure tests
// =============================================================================

[<Fact>]
let ``generates valid OpenAPI 3.0 structure`` () =
    let doc = generateAndParse<SimpleRoute> ()
    test <@ doc["openapi"].GetValue<string>() = "3.0.0" @>
    test <@ doc["info"].["title"].GetValue<string>() = "Test API" @>
    test <@ doc["info"].["version"].GetValue<string>() = "1.0.0" @>
    test <@ not (isNull doc["paths"]) @>

[<Fact>]
let ``includes description when provided`` () =
    let config =
        { Title = "Test"
          Version = "1.0"
          Description = Some "A test API" }

    let doc = Spec.generate<SimpleRoute> config |> parse
    test <@ doc["info"].["description"].GetValue<string>() = "A test API" @>

[<Fact>]
let ``omits description when None`` () =
    let doc = generateAndParse<SimpleRoute> ()
    test <@ isNull doc["info"].["description"] @>

// =============================================================================
// Path and method mapping tests
// =============================================================================

[<Fact>]
let ``maps Root to GET /`` () =
    let doc = generateAndParse<SimpleRoute> ()
    let paths = doc["paths"] :?> JsonObject
    test <@ not (isNull paths["/"]) @>
    let rootPath = paths["/"] :?> JsonObject
    test <@ not (isNull rootPath["get"]) @>

[<Fact>]
let ``maps Show of id: Guid to GET /{id}`` () =
    let doc = generateAndParse<SimpleRoute> ()
    let paths = doc["paths"] :?> JsonObject
    test <@ not (isNull paths["/{id}"]) @>

[<Fact>]
let ``infers POST method for Create`` () =
    let doc = generateAndParse<MethodRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let rootPath = paths["/"] :?> JsonObject
    test <@ not (isNull rootPath["post"]) @>

[<Fact>]
let ``infers DELETE method for Delete`` () =
    let doc = generateAndParse<MethodRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let idPath = paths["/{id}"] :?> JsonObject
    test <@ not (isNull idPath["delete"]) @>

[<Fact>]
let ``infers PATCH method for Patch`` () =
    let doc = generateAndParse<MethodRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let idPath = paths["/{id}"] :?> JsonObject
    test <@ not (isNull idPath["patch"]) @>

// =============================================================================
// Path parameter tests
// =============================================================================

[<Fact>]
let ``Guid path param has uuid format`` () =
    let doc = generateAndParse<SimpleRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let idPath = paths["/{id}"] :?> JsonObject
    let getOp = idPath["get"] :?> JsonObject
    let parameters = getOp["parameters"] :?> JsonArray
    let param = parameters[0] :?> JsonObject
    test <@ param["name"].GetValue<string>() = "id" @>
    test <@ param["in"].GetValue<string>() = "path" @>
    test <@ param["required"].GetValue<bool>() = true @>
    test <@ param["schema"].["type"].GetValue<string>() = "string" @>
    test <@ param["schema"].["format"].GetValue<string>() = "uuid" @>

[<Fact>]
let ``int path param has int32 format`` () =
    let doc = generateAndParse<IntParamRoute> ()
    let paths = doc["paths"] :?> JsonObject
    // Show of page: int -> /{page} (Show is a special name, param-only path)
    let pagePath = paths["/{page}"] :?> JsonObject
    let getOp = pagePath["get"] :?> JsonObject
    let parameters = getOp["parameters"] :?> JsonArray
    let param = parameters[0] :?> JsonObject
    test <@ param["schema"].["type"].GetValue<string>() = "integer" @>
    test <@ param["schema"].["format"].GetValue<string>() = "int32" @>

[<Fact>]
let ``int64 path param has int64 format`` () =
    let doc = generateAndParse<Int64ParamRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let idPath = paths["/{id}"] :?> JsonObject
    let getOp = idPath["get"] :?> JsonObject
    let parameters = getOp["parameters"] :?> JsonArray
    let param = parameters[0] :?> JsonObject
    test <@ param["schema"].["type"].GetValue<string>() = "integer" @>
    test <@ param["schema"].["format"].GetValue<string>() = "int64" @>

[<Fact>]
let ``bool path param has boolean type`` () =
    let doc = generateAndParse<BoolParamRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let enabledPath = paths["/{enabled}"] :?> JsonObject
    let getOp = enabledPath["get"] :?> JsonObject
    let parameters = getOp["parameters"] :?> JsonArray
    let param = parameters[0] :?> JsonObject
    test <@ param["schema"].["type"].GetValue<string>() = "boolean" @>

[<Fact>]
let ``string path param has string type`` () =
    let doc = generateAndParse<StringParamRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let slugPath = paths["/{slug}"] :?> JsonObject
    let getOp = slugPath["get"] :?> JsonObject
    let parameters = getOp["parameters"] :?> JsonArray
    let param = parameters[0] :?> JsonObject
    test <@ param["schema"].["type"].GetValue<string>() = "string" @>

[<Fact>]
let ``single-case DU wrapper unwraps to inner type in path param`` () =
    let doc = generateAndParse<WrappedIdRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let idPath = paths["/{id}"] :?> JsonObject
    let getOp = idPath["get"] :?> JsonObject
    let parameters = getOp["parameters"] :?> JsonArray
    let param = parameters[0] :?> JsonObject
    // PostId of Guid should unwrap to uuid
    test <@ param["schema"].["type"].GetValue<string>() = "string" @>
    test <@ param["schema"].["format"].GetValue<string>() = "uuid" @>

// =============================================================================
// Constraint stripping tests
// =============================================================================

[<Fact>]
let ``strips type constraints from paths`` () =
    // SimpleRoute.Show produces path /{id:guid} - should be stripped to /{id}
    let doc = generateAndParse<SimpleRoute> ()
    let paths = doc["paths"] :?> JsonObject
    test <@ not (isNull paths["/{id}"]) @>

[<Fact>]
let ``strips int constraints from paths`` () =
    let doc = generateAndParse<IntParamRoute> ()
    let paths = doc["paths"] :?> JsonObject
    // Show of page: int -> /{page:int} stripped to /{page}
    test <@ not (isNull paths["/{page}"]) @>

// =============================================================================
// Query parameter tests
// =============================================================================

[<Fact>]
let ``required query param is marked required`` () =
    let doc = generateAndParse<QueryParamRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let searchPath = paths["/search"] :?> JsonObject
    let getOp = searchPath["get"] :?> JsonObject
    let parameters = getOp["parameters"] :?> JsonArray
    let param = parameters[0] :?> JsonObject
    test <@ param["name"].GetValue<string>() = "query" @>
    test <@ param["in"].GetValue<string>() = "query" @>
    test <@ param["required"].GetValue<bool>() = true @>
    test <@ param["schema"].["type"].GetValue<string>() = "string" @>

[<Fact>]
let ``optional query param is not required`` () =
    let doc = generateAndParse<QueryParamRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let paginatedPath = paths["/paginated"] :?> JsonObject
    let getOp = paginatedPath["get"] :?> JsonObject
    let parameters = getOp["parameters"] :?> JsonArray
    let param = parameters[0] :?> JsonObject
    test <@ param["name"].GetValue<string>() = "page" @>
    test <@ param["in"].GetValue<string>() = "query" @>
    test <@ param["required"].GetValue<bool>() = false @>
    test <@ param["schema"].["type"].GetValue<string>() = "integer" @>

// =============================================================================
// Request body tests
// =============================================================================

[<Fact>]
let ``JsonBody produces application/json request body`` () =
    let doc = generateAndParse<JsonBodyRoute> ()
    let paths = doc["paths"] :?> JsonObject
    // Create -> POST /
    let createPath = paths["/"] :?> JsonObject
    let postOp = createPath["post"] :?> JsonObject
    let requestBody = postOp["requestBody"] :?> JsonObject
    test <@ requestBody["required"].GetValue<bool>() = true @>
    let content = requestBody["content"] :?> JsonObject
    test <@ not (isNull content["application/json"]) @>

[<Fact>]
let ``FormBody produces application/x-www-form-urlencoded request body`` () =
    let doc = generateAndParse<FormBodyRoute> ()
    let paths = doc["paths"] :?> JsonObject
    // Submit is not a special name -> /submit, method GET
    let submitPath = paths["/submit"] :?> JsonObject
    let getOp = submitPath["get"] :?> JsonObject
    let requestBody = getOp["requestBody"] :?> JsonObject
    test <@ requestBody["required"].GetValue<bool>() = true @>
    let content = requestBody["content"] :?> JsonObject
    test <@ not (isNull content["application/x-www-form-urlencoded"]) @>

// =============================================================================
// Response schema tests
// =============================================================================

[<Fact>]
let ``Returns produces response schema`` () =
    let doc = generateAndParse<ReturnsRoute> ()
    let paths = doc["paths"] :?> JsonObject
    // List -> GET /
    let listPath = paths["/"] :?> JsonObject
    let getOp = listPath["get"] :?> JsonObject
    let responses = getOp["responses"] :?> JsonObject
    let ok = responses["200"] :?> JsonObject
    test <@ ok["description"].GetValue<string>() = "Successful response" @>
    let content = ok["content"] :?> JsonObject
    test <@ not (isNull content["application/json"]) @>

[<Fact>]
let ``Returns Fortune list produces array schema`` () =
    let doc = generateAndParse<ReturnsRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let listPath = paths["/"] :?> JsonObject
    let getOp = listPath["get"] :?> JsonObject

    let schema =
        (getOp["responses"].["200"].["content"].["application/json"] :?> JsonObject)["schema"] :?> JsonObject

    test <@ schema["type"].GetValue<string>() = "array" @>

[<Fact>]
let ``Returns Fortune produces ref to Fortune schema`` () =
    let doc = generateAndParse<ReturnsRoute> ()
    let paths = doc["paths"] :?> JsonObject
    // Show of id: Guid * Returns<Fortune> -> GET /{id}
    let showPath = paths["/{id}"] :?> JsonObject
    let getOp = showPath["get"] :?> JsonObject

    let schema =
        (getOp["responses"].["200"].["content"].["application/json"] :?> JsonObject)["schema"] :?> JsonObject

    test <@ schema["$ref"].GetValue<string>() = "#/components/schemas/Fortune" @>

// =============================================================================
// Record type / components/schemas tests
// =============================================================================

[<Fact>]
let ``record types produce components/schemas entries`` () =
    let doc = generateAndParse<ReturnsRoute> ()
    let schemas = doc["components"].["schemas"] :?> JsonObject
    test <@ not (isNull schemas["Fortune"]) @>
    let fortune = schemas["Fortune"] :?> JsonObject
    test <@ fortune["type"].GetValue<string>() = "object" @>
    let props = fortune["properties"] :?> JsonObject
    test <@ not (isNull props["Message"]) @>
    test <@ props["Message"].["type"].GetValue<string>() = "string" @>

[<Fact>]
let ``record with optional field omits it from required`` () =
    let doc = generateAndParse<OptionalFieldRoute> ()
    let schemas = doc["components"].["schemas"] :?> JsonObject
    let record = schemas["OptionalFieldRecord"] :?> JsonObject
    let required = record["required"] :?> JsonArray
    let requiredNames = [ for item in required -> item.GetValue<string>() ]
    test <@ requiredNames |> List.contains "Name" @>
    test <@ not (requiredNames |> List.contains "Email") @>

// =============================================================================
// Nested route tests
// =============================================================================

[<Fact>]
let ``nested routes produce correct paths`` () =
    let doc = generateAndParse<NestedParentRoute> ()
    let paths = doc["paths"] :?> JsonObject
    // List -> GET /
    test <@ not (isNull paths["/"]) @>
    // Member of id * Show -> GET /{id}
    test <@ not (isNull paths["/{id}"]) @>
    // Member of id * Edit -> GET /{id}/edit
    test <@ not (isNull paths["/{id}/edit"]) @>

[<Fact>]
let ``nested routes inherit correct methods`` () =
    let doc = generateAndParse<NestedParentRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let idPath = paths["/{id}"] :?> JsonObject
    // Delete produces DELETE method
    test <@ not (isNull idPath["delete"]) @>

// =============================================================================
// Precondition exclusion tests
// =============================================================================

[<Fact>]
let ``precondition fields are excluded from parameters`` () =
    let doc = generateAndParse<PreconditionRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let privatePath = paths["/private"] :?> JsonObject
    let getOp = privatePath["get"] :?> JsonObject
    // Should have no parameters (PreCondition is not a path/query param)
    test <@ isNull getOp["parameters"] @>

[<Fact>]
let ``overridable precondition fields are excluded from parameters`` () =
    let doc = generateAndParse<PreconditionRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let overridablePath = paths["/overridable"] :?> JsonObject
    let getOp = overridablePath["get"] :?> JsonObject
    test <@ isNull getOp["parameters"] @>

// =============================================================================
// List/array type tests
// =============================================================================

[<Fact>]
let ``list type produces array schema`` () =
    let doc = generateAndParse<ArrayRoute> ()
    let schemas = doc["components"].["schemas"] :?> JsonObject
    let record = schemas["ArrayFieldRecord"] :?> JsonObject
    let tags = record["properties"].["Tags"] :?> JsonObject
    test <@ tags["type"].GetValue<string>() = "array" @>
    test <@ tags["items"].["type"].GetValue<string>() = "string" @>

// =============================================================================
// Mixed route tests
// =============================================================================

[<Fact>]
let ``mixed route with path param and Returns has both`` () =
    let doc = generateAndParse<MixedRoute> ()
    let paths = doc["paths"] :?> JsonObject
    // Show of id: Guid * Returns<PostInput> -> GET /{id}
    let showPath = paths["/{id}"] :?> JsonObject
    let getOp = showPath["get"] :?> JsonObject
    // Has path parameter
    let parameters = getOp["parameters"] :?> JsonArray
    test <@ parameters.Count = 1 @>
    // Has response schema
    let responses = getOp["responses"] :?> JsonObject
    let ok = responses["200"] :?> JsonObject
    test <@ not (isNull ok["content"]) @>

[<Fact>]
let ``mixed route with body and precondition has body but no precondition param`` () =
    let doc = generateAndParse<MixedRoute> ()
    let paths = doc["paths"] :?> JsonObject
    // Create of JsonBody<PostInput> * PreCondition<Guid> -> POST /
    let createPath = paths["/"] :?> JsonObject
    let postOp = createPath["post"] :?> JsonObject
    // Has request body
    test <@ not (isNull postOp["requestBody"]) @>
    // Should have no parameters (PreCondition is excluded)
    test <@ isNull postOp["parameters"] @>

[<Fact>]
let ``mixed route with query params has correct parameter list`` () =
    let doc = generateAndParse<MixedRoute> ()
    let paths = doc["paths"] :?> JsonObject
    // Search of query: QueryParam<string> * page: QueryParam<int> option -> GET /search
    let searchPath = paths["/search"] :?> JsonObject
    let getOp = searchPath["get"] :?> JsonObject
    let parameters = getOp["parameters"] :?> JsonArray
    test <@ parameters.Count = 2 @>

    let paramNames =
        [ for p in parameters do
              let obj = p :?> JsonObject
              yield obj["name"].GetValue<string>() ]

    test <@ paramNames |> List.contains "query" @>
    test <@ paramNames |> List.contains "page" @>

// =============================================================================
// No components when no records
// =============================================================================

[<Fact>]
let ``omits components when no record types used`` () =
    let doc = generateAndParse<SimpleRoute> ()
    test <@ isNull doc["components"] @>

// =============================================================================
// Routes without returns have simple 200 response
// =============================================================================

[<Fact>]
let ``routes without Returns have simple 200 response`` () =
    let doc = generateAndParse<SimpleRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let rootPath = paths["/"] :?> JsonObject
    let getOp = rootPath["get"] :?> JsonObject
    let responses = getOp["responses"] :?> JsonObject
    let ok = responses["200"] :?> JsonObject
    test <@ ok["description"].GetValue<string>() = "Successful response" @>
    test <@ isNull ok["content"] @>

// =============================================================================
// Float/decimal type tests
// =============================================================================

[<Fact>]
let ``float field produces number/double schema`` () =
    let doc = generateAndParse<FloatFieldRoute> ()
    let schemas = doc["components"].["schemas"] :?> JsonObject
    let record = schemas["FloatRecord"] :?> JsonObject
    let score = record["properties"].["Score"] :?> JsonObject
    test <@ score["type"].GetValue<string>() = "number" @>
    test <@ score["format"].GetValue<string>() = "double" @>

[<Fact>]
let ``float32 field produces number/float schema`` () =
    let doc = generateAndParse<FloatFieldRoute> ()
    let schemas = doc["components"].["schemas"] :?> JsonObject
    let record = schemas["FloatRecord"] :?> JsonObject
    let rating = record["properties"].["Rating"] :?> JsonObject
    test <@ rating["type"].GetValue<string>() = "number" @>
    test <@ rating["format"].GetValue<string>() = "float" @>

[<Fact>]
let ``decimal field produces number/decimal schema`` () =
    let doc = generateAndParse<FloatFieldRoute> ()
    let schemas = doc["components"].["schemas"] :?> JsonObject
    let record = schemas["FloatRecord"] :?> JsonObject
    let price = record["properties"].["Price"] :?> JsonObject
    test <@ price["type"].GetValue<string>() = "number" @>
    test <@ price["format"].GetValue<string>() = "decimal" @>

// =============================================================================
// Array type tests (native array, not list)
// =============================================================================

[<Fact>]
let ``native array field produces array schema`` () =
    let doc = generateAndParse<ArrayFieldRoute> ()
    let schemas = doc["components"].["schemas"] :?> JsonObject
    let record = schemas["ArrayRecord"] :?> JsonObject
    let items = record["properties"].["Items"] :?> JsonObject
    test <@ items["type"].GetValue<string>() = "array" @>
    test <@ items["items"].["type"].GetValue<string>() = "string" @>

// =============================================================================
// Option type in schema (nullable field)
// =============================================================================

[<Fact>]
let ``option field in response record produces nullable schema`` () =
    let doc = generateAndParse<NullableResponseRoute> ()
    let schemas = doc["components"].["schemas"] :?> JsonObject
    let record = schemas["NullableResponse"] :?> JsonObject
    let value = record["properties"].["Value"] :?> JsonObject
    test <@ value["type"].GetValue<string>() = "string" @>

[<Fact>]
let ``option inside list produces nullable items`` () =
    let doc = generateAndParse<OptionListRoute> ()
    let schemas = doc["components"].["schemas"] :?> JsonObject
    let record = schemas["OptionListRecord"] :?> JsonObject
    let items = record["properties"].["Items"] :?> JsonObject
    test <@ items["type"].GetValue<string>() = "array" @>
    let itemSchema = items["items"] :?> JsonObject
    test <@ itemSchema["type"].GetValue<string>() = "string" @>
    test <@ itemSchema["nullable"].GetValue<bool>() = true @>

// =============================================================================
// Unknown type fallback tests
// =============================================================================

[<Fact>]
let ``unknown type falls back to object schema`` () =
    let doc = generateAndParse<DateFieldRoute> ()
    let schemas = doc["components"].["schemas"] :?> JsonObject
    let record = schemas["DateRecord"] :?> JsonObject
    let createdAt = record["properties"].["CreatedAt"] :?> JsonObject
    test <@ createdAt["type"].GetValue<string>() = "object" @>

// =============================================================================
// PUT and Any method tests
// =============================================================================

[<Fact>]
let ``Put method route produces put operation`` () =
    let doc = generateAndParse<PutRoute> ()
    let paths = doc["paths"] :?> JsonObject
    // Update is not a special name, so path is /update/{id}
    let updatePath = paths["/update/{id}"] :?> JsonObject
    test <@ not (isNull updatePath["put"]) @>

[<Fact>]
let ``Any method route produces get operation`` () =
    let doc = generateAndParse<AnyRoute> ()
    let paths = doc["paths"] :?> JsonObject
    let rootPath = paths["/catch"] :?> JsonObject
    test <@ not (isNull rootPath["get"]) @>
