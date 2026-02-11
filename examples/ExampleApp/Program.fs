// Run with: mise run example

open System
open ExampleApp
open Falco
open Falco.Markup
open Falco.Routing
open Falco.UnionRoutes
open System.Text.RegularExpressions
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection

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
    | Create of JsonBody<PostInput> * PreCondition<UserId>
    | Search of query: QueryParam<string>
    | Member of id: Guid * PostDetailRoute

type ItemRoute =
    | List
    | [<SkipAllPreconditions>] Public
    | [<SkipPrecondition(typeof<UserId>)>] Limited

type AdminRoute = Dashboard of PreCondition<AdminId>

type HealthResponse = { Status: string }

type Route =
    | Root
    | Posts of PostRoute
    | [<Route(Path = "articles/{slug}")>] Article of slug: Slug
    | [<Route(RouteMethod.Put, Path = "settings")>] UpdateSettings of PreCondition<UserId>
    | Items of userId: UserId * OverridablePreCondition<UserId> * ItemRoute
    | [<Route(Path = "")>] Admin of AdminRoute
    | [<Route(RouteMethod.Post, Path = "contact")>] Contact of FormBody<ContactInput>
    | Health of Returns<HealthResponse>
    | [<Route(Constraints = [| RouteConstraint.Alpha |], MinLength = 3, MaxLength = 50)>] Tag of name: string

// =============================================================================
// Custom parser
// =============================================================================

let slugParser =
    Extractor.constrainedParser<Slug> [| RouteConstraint.Alpha |] (fun s -> Ok(Slug s))

// =============================================================================
// Endpoint configuration
// =============================================================================

let endpointConfig: EndpointConfig<AppError> =
    { Preconditions =
        [ Extractor.precondition<UserId, AppError> Handlers.requireAuth
          Extractor.overridablePrecondition<UserId, AppError> Handlers.requireAuth
          Extractor.precondition<AdminId, AppError> Handlers.requireAdmin ]
      Parsers = [ slugParser ]
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

/// Replace {param} or {param:constraint} placeholders with sample values for browsable links
let makeBrowsable (route: Route) (path: string) =
    let replaceParam (name: string) (value: string) (p: string) =
        Regex.Replace(p, @"\{" + Regex.Escape(name) + @"(:[^}]*)?\}", value)

    let path =
        path
        |> replaceParam "id" (sampleId.ToString())
        |> replaceParam "userId" (sampleId.ToString())
        |> replaceParam "slug" "hello-world"
        |> replaceParam "name" "fsharp"

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

          Elem.p
              []
              [ Elem.a [ Attr.href "/login" ] [ Text.raw "Log in" ]
                Text.raw " | "
                Elem.form
                    [ Attr.method "post"; Attr.action "/logout"; Attr.style "display:inline" ]
                    [ Elem.input [ Attr.type' "submit"; Attr.value "Log out" ] ] ]

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
Route.link (Health(Returns())) = "{Route.link (Health(Returns()))}"
Route.link (Posts (Member ({sampleId}, Show))) = "{Route.link (Posts(Member(sampleId, Show)))}"
Route.link (Article (Slug "hello-world")) = "{Route.link (Article(Slug "hello-world"))}"
Route.link (Items (UserId {sampleId}, OverridablePreCondition (UserId {sampleId}), List)) = "{Route.link (Items(UserId sampleId, OverridablePreCondition(UserId sampleId), ItemRoute.List))}"
Route.link (Tag "fsharp") = "{Route.link (Tag "fsharp")}" """ ]

          // curl examples
          Elem.h2 [] [ Text.raw "Try with curl" ]
          Elem.pre
              []
              [ Text.raw
                    $"""# Log in first (creates auth cookie)
curl -c cookies.txt -X POST http://localhost:5000/login -d "userId={sampleId}&isAdmin=true"

# Create post (requires auth cookie + JSON body)
curl -b cookies.txt -X POST http://localhost:5000/posts -H "Content-Type: application/json" -d '{{"Title":"Hello","Body":"World"}}'

# Update settings (PUT, requires auth)
curl -b cookies.txt -X PUT http://localhost:5000/settings

# Delete post
curl -X DELETE http://localhost:5000/posts/{sampleId}

# Patch post
curl -X PATCH http://localhost:5000/posts/{sampleId}

# Submit contact form (FormBody extraction)
curl -X POST http://localhost:5000/contact -d "Name=Alice&Message=Hello" """ ] ]
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
    | PostRoute.Create(JsonBody input, PreCondition userId) -> Handlers.postCreate input userId
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
    | Route.Admin(AdminRoute.Dashboard(PreCondition adminId)) -> Handlers.dashboard adminId
    | Route.Contact(FormBody input) -> Handlers.contactSubmit input
    | Route.Health returns -> Route.respond returns { Status = "ok" }
    | Route.Tag name -> Handlers.tag name

// =============================================================================
// Entry point
// =============================================================================

let endpoints = Route.endpoints endpointConfig handleRoute

let authEndpoints =
    [ get "/login" Handlers.loginPage
      post "/login" Handlers.loginSubmit
      post "/logout" Handlers.logoutSubmit ]

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    builder.Services.AddAuthentication(CookieAuthenticationDefaults.AuthenticationScheme).AddCookie()
    |> ignore

    let app = builder.Build()
    app.UseAuthentication() |> ignore
    app.UseRouting() |> ignore
    app.UseFalco(authEndpoints @ endpoints) |> ignore
    app.Run()
    0
