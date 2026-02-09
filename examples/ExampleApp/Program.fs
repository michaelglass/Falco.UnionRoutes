// Run with: mise run example

open System
open ExampleApp
open Falco
open Falco.Markup
open Falco.UnionRoutes
open Microsoft.AspNetCore.Builder

// =============================================================================
// Route Definitions
// =============================================================================

type PostDetailRoute =
    | Show
    | Edit
    | Delete
    | Patch

type PostRoute =
    | List of page: QueryParam<int> option
    | Create of PreCondition<UserId>
    | Search of query: QueryParam<string>
    | Member of id: Guid * PostDetailRoute

type ItemRoute =
    | List
    | [<SkipAllPreconditions>] Public
    | [<SkipPrecondition(typeof<UserId>)>] Limited

type AdminRoute = Dashboard of PreCondition<UserId>

type Route =
    | Root
    | Posts of PostRoute
    | [<Route(Path = "articles/{slug}")>] Article of slug: Slug
    | [<Route(RouteMethod.Put, Path = "settings")>] UpdateSettings of PreCondition<UserId>
    | Items of userId: UserId * OverridablePreCondition<UserId> * ItemRoute
    | [<Route(Path = "")>] Admin of AdminRoute
    | Health

// =============================================================================
// Custom parser
// =============================================================================

let slugParser: Parser<Slug> = fun s -> Ok(Slug s)

// =============================================================================
// Endpoint configuration
// =============================================================================

let endpointConfig: EndpointConfig<AppError> =
    { Preconditions =
        [ Extractor.precondition<UserId, AppError> Handlers.requireAuth
          Extractor.overridablePrecondition<UserId, AppError> Handlers.requireAuth ]
      Parsers = [ Extractor.parser slugParser ]
      MakeError = fun msg -> BadRequest msg
      CombineErrors =
        fun errors ->
            match errors with
            | [ single ] -> single
            | multiple -> BadRequest(multiple |> List.map string |> String.concat "; ")
      ToErrorResponse = Handlers.toErrorResponse }

// =============================================================================
// Home page — demonstrates Route.allRoutes, Route.info, Route.link
// =============================================================================

let sampleId = Guid.Parse("11111111-1111-1111-1111-111111111111")

/// Replace {param} placeholders with sample values for browsable links
let makeBrowsable (route: Route) (path: string) =
    let path =
        path
            .Replace("{id}", sampleId.ToString())
            .Replace("{userId}", sampleId.ToString())
            .Replace("{slug}", "hello-world")

    match route with
    | Posts(Search _) -> path + "?query=hello"
    | _ -> path

let home: HttpHandler =
    let allRoutes = Route.allRoutes<Route> ()

    Handlers.layout
        "Falco.UnionRoutes Example"
        [ Elem.h1 [] [ Text.raw "Falco.UnionRoutes Example" ]
          Elem.p
              []
              [ Text.raw $"Discovered {allRoutes.Length} routes via "
                Elem.code [] [ Text.raw "Route.allRoutes<Route>()" ] ]

          // Route table via Route.info
          Elem.h2 [] [ Text.raw "All routes" ]
          Elem.table
              []
              [ Elem.thead
                    []
                    [ Elem.tr
                          []
                          [ Elem.th [] [ Text.raw "Method" ]
                            Elem.th [] [ Text.raw "Path" ]
                            Elem.th [] [ Text.raw "Link" ] ] ]
                Elem.tbody
                    []
                    (allRoutes
                     |> List.map (fun route ->
                         let info = Route.info route
                         let methodStr = info.Method.ToString().ToUpper()
                         let browsable = makeBrowsable route info.Path

                         Elem.tr
                             []
                             [ Elem.td [] [ Elem.code [] [ Text.raw methodStr ] ]
                               Elem.td [] [ Elem.code [] [ Text.raw info.Path ] ]
                               Elem.td
                                   []
                                   [ if info.Method = HttpMethod.Get then
                                         Elem.a [ Attr.href browsable ] [ Text.raw browsable ]
                                     else
                                         Text.raw "(use curl)" ] ])) ]

          // Route.link examples
          Elem.h2 [] [ Text.raw "Route.link examples" ]
          Elem.pre
              []
              [ Text.raw
                    $"""Route.link Root = "{Route.link Root}"
Route.link Health = "{Route.link Health}"
Route.link (Posts (Member ({sampleId}, Show))) = "{Route.link (Posts(Member(sampleId, Show)))}"
Route.link (Article (Slug "hello-world")) = "{Route.link (Article(Slug "hello-world"))}"
Route.link (Items (UserId {sampleId}, OverridablePreCondition (UserId {sampleId}), List)) = "{Route.link (Items(UserId sampleId, OverridablePreCondition(UserId sampleId), ItemRoute.List))}" """ ]

          // curl examples
          Elem.h2 [] [ Text.raw "Try with curl" ]
          Elem.pre
              []
              [ Text.raw
                    $"""# Create post (requires X-User-Id header)
curl -X POST http://localhost:5000/posts -H "X-User-Id: {sampleId}"

# Update settings (PUT, requires auth)
curl -X PUT http://localhost:5000/settings -H "X-User-Id: {sampleId}"

# Delete post
curl -X DELETE http://localhost:5000/posts/{sampleId}

# Patch post
curl -X PATCH http://localhost:5000/posts/{sampleId}" """ ] ]
    |> Response.ofHtml

// =============================================================================
// Route handlers — pattern match on hydrated values
// =============================================================================

let handlePostDetail (id: Guid) (route: PostDetailRoute) : HttpHandler =
    match route with
    | PostDetailRoute.Show -> Handlers.postShow id
    | PostDetailRoute.Edit -> Handlers.postEdit id
    | PostDetailRoute.Delete -> Handlers.postDelete id
    | PostDetailRoute.Patch -> Handlers.postPatch id

let handlePost (route: PostRoute) : HttpHandler =
    match route with
    | PostRoute.List page -> Handlers.postList page
    | PostRoute.Create(PreCondition userId) -> Handlers.postCreate userId
    | PostRoute.Search query -> Handlers.postSearch query
    | PostRoute.Member(id, detail) -> handlePostDetail id detail

let handleItem (userId: UserId) (route: ItemRoute) : HttpHandler =
    match route with
    | ItemRoute.List -> Handlers.itemList userId
    | ItemRoute.Public -> Handlers.itemPublic userId
    | ItemRoute.Limited -> Handlers.itemLimited userId

let handleRoute (route: Route) : HttpHandler =
    match route with
    | Route.Root -> home
    | Route.Posts postRoute -> handlePost postRoute
    | Route.Article slug -> Handlers.article slug
    | Route.UpdateSettings(PreCondition userId) -> Handlers.updateSettings userId
    | Route.Items(userId, _, itemRoute) -> handleItem userId itemRoute
    | Route.Admin(AdminRoute.Dashboard(PreCondition userId)) -> Handlers.dashboard userId
    | Route.Health -> Handlers.health

// =============================================================================
// Entry point
// =============================================================================

let endpoints = Route.endpoints endpointConfig handleRoute

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    let app = builder.Build()
    app.UseRouting() |> ignore
    app.UseFalco(endpoints) |> ignore
    app.Run()
    0
