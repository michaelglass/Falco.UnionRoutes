namespace ExampleApp

open System
open System.Security.Claims
open System.Threading.Tasks
open Falco
open Falco.Markup
open Falco.UnionRoutes
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.Cookies

// =============================================================================
// Domain Types
// =============================================================================

type UserId = UserId of Guid
type AdminId = AdminId of Guid
type Slug = Slug of string

type PostInput = { Title: string; Body: string }
type ContactInput = { Name: string; Message: string }

type AppError =
    | NotAuthenticated
    | Forbidden of string
    | BadRequest of string

// =============================================================================
// Handlers
// =============================================================================

module Handlers =

    // -------------------------------------------------------------------------
    // HTML helpers
    // -------------------------------------------------------------------------

    let layout (title: string) (content: XmlNode list) =
        Elem.html
            []
            [ Elem.head
                  []
                  [ Elem.title [] [ Text.raw title ]
                    Elem.style
                        []
                        [ Text.raw
                              """
                body { font-family: system-ui, sans-serif; max-width: 800px; margin: 2rem auto; padding: 0 1rem; }
                h1 { color: #333; } h2 { color: #555; }
                a { color: #0066cc; }
                code { background: #f4f4f4; padding: 0.2rem 0.4rem; border-radius: 3px; }
                .error { color: #cc0000; }
                pre { background: #f4f4f4; padding: 1rem; overflow-x: auto; }
                table { border-collapse: collapse; width: 100%; }
                th, td { border: 1px solid #ddd; padding: 0.5rem; text-align: left; }
                th { background: #f4f4f4; }
                """ ] ]
              Elem.body [] content ]

    /// Renders a feature-demonstration page with heading, extracted values, feature list, and back link.
    let featurePage (title: string) (values: (string * string) list) (features: string list) : HttpHandler =
        let content =
            [ Elem.h1 [] [ Text.raw title ]

              if not values.IsEmpty then
                  Elem.h2 [] [ Text.raw "Extracted values" ]

                  Elem.ul
                      []
                      (values
                       |> List.map (fun (k, v) ->
                           Elem.li [] [ Elem.strong [] [ Text.raw $"{k}: " ]; Elem.code [] [ Text.raw v ] ]))

              Elem.h2 [] [ Text.raw "Features demonstrated" ]
              Elem.ul [] (features |> List.map (fun f -> Elem.li [] [ Text.raw f ]))
              Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]

        layout title content |> Response.ofHtml

    // -------------------------------------------------------------------------
    // Error response
    // -------------------------------------------------------------------------

    /// Converts AppError to HTTP response.
    let toErrorResponse (error: AppError) : HttpHandler =
        match error with
        | NotAuthenticated ->
            Response.withStatusCode 401
            >> Response.ofHtml (
                layout
                    "Error 401"
                    [ Elem.h1 [ Attr.class' "error" ] [ Text.raw "401 Unauthorized" ]
                      Elem.p [] [ Text.raw "Please log in." ]
                      Elem.a [ Attr.href "/login" ] [ Text.raw "Log in" ] ]
            )
        | Forbidden msg ->
            Response.withStatusCode 403
            >> Response.ofHtml (
                layout
                    "Error 403"
                    [ Elem.h1 [ Attr.class' "error" ] [ Text.raw "403 Forbidden" ]
                      Elem.p [] [ Text.raw msg ]
                      Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]
            )
        | BadRequest msg ->
            Response.withStatusCode 400
            >> Response.ofHtml (
                layout
                    "Error 400"
                    [ Elem.h1 [ Attr.class' "error" ] [ Text.raw "400 Bad Request" ]
                      Elem.p [] [ Text.raw msg ]
                      Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]
            )

    // -------------------------------------------------------------------------
    // Auth extractors
    // -------------------------------------------------------------------------

    /// Extracts UserId from ClaimsPrincipal (NameIdentifier claim).
    let requireAuth: Extractor<UserId, AppError> =
        fun ctx ->
            Task.FromResult(
                match ctx.User.FindFirst(ClaimTypes.NameIdentifier) with
                | null -> Error NotAuthenticated
                | claim ->
                    match Guid.TryParse(claim.Value) with
                    | true, guid -> Ok(UserId guid)
                    | false, _ -> Error NotAuthenticated
            )

    /// Extracts AdminId from ClaimsPrincipal (requires Admin role).
    let requireAdmin: Extractor<AdminId, AppError> =
        fun ctx ->
            Task.FromResult(
                match ctx.User.FindFirst(ClaimTypes.NameIdentifier) with
                | null -> Error NotAuthenticated
                | claim ->
                    if ctx.User.IsInRole("Admin") then
                        match Guid.TryParse(claim.Value) with
                        | true, guid -> Ok(AdminId guid)
                        | false, _ -> Error NotAuthenticated
                    else
                        Error(Forbidden "Admin role required.")
            )

    // -------------------------------------------------------------------------
    // Individual handlers
    // -------------------------------------------------------------------------

    /// GET /posts — List convention + optional QueryParam
    let postList (page: QueryParam<int> option) : HttpHandler =
        let pageNum = page |> Option.map (fun (QueryParam p) -> p) |> Option.defaultValue 1

        featurePage
            "Post List"
            [ ("page", string pageNum) ]
            [ "List convention — GET with no path segment"
              "QueryParam&lt;int&gt; option — optional query parameter" ]

    /// POST /posts — Create/POST convention + JsonBody + PreCondition
    let postCreate (input: PostInput) (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        featurePage
            "Post Created"
            [ ("userId", string uid); ("title", input.Title); ("body", input.Body) ]
            [ "Create convention — POST method inferred from case name"
              "JsonBody&lt;PostInput&gt; — request body deserialized from JSON"
              "PreCondition&lt;UserId&gt; — value extracted via precondition" ]

    /// GET /posts/search — Default kebab-case + required QueryParam
    let postSearch (query: QueryParam<string>) : HttpHandler =
        let (QueryParam q) = query

        featurePage
            "Post Search"
            [ ("query", q) ]
            [ "Default kebab-case path — Search becomes /search"
              "QueryParam&lt;string&gt; — required query parameter" ]

    /// GET /posts/{id:guid} — Show convention with implicit :guid constraint
    let postShow (id: Guid) : HttpHandler =
        featurePage
            "Post Detail"
            [ ("id", string id) ]
            [ "Show convention — param-only path, no case-name segment"
              "Guid field → implicit :guid constraint (non-GUIDs rejected by ASP.NET Core)" ]

    /// GET /posts/{id}/edit — Edit convention
    let postEdit (id: Guid) : HttpHandler =
        featurePage "Post Edit" [ ("id", string id) ] [ "Edit convention — produces /edit path segment" ]

    /// DELETE /posts/{id} — Delete convention
    let postDelete (id: Guid) : HttpHandler =
        featurePage "Post Deleted" [ ("id", string id) ] [ "Delete convention — DELETE method inferred from case name" ]

    /// PATCH /posts/{id} — Patch convention
    let postPatch (id: Guid) : HttpHandler =
        featurePage "Post Patched" [ ("id", string id) ] [ "Patch convention — PATCH method inferred from case name" ]

    /// GET /items/{userId:guid} — inherits OverridablePreCondition
    let itemList (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        featurePage
            "Item List"
            [ ("userId", string uid) ]
            [ "OverridablePreCondition&lt;UserId&gt; — precondition inherited from parent"
              "Single-case DU wrapper — UserId of Guid → implicit :guid constraint" ]

    /// GET /items/{userId}/public — SkipAllPreconditions
    let itemPublic (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        featurePage
            "Public Items"
            [ ("userId", string uid) ]
            [ "[&lt;SkipAllPreconditions&gt;] — all OverridablePreCondition skipped"
              "No authentication required for this route" ]

    /// GET /items/{userId}/limited — SkipPrecondition(typeof<UserId>)
    let itemLimited (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        featurePage
            "Limited Items"
            [ ("userId", string uid) ]
            [ "[&lt;SkipPrecondition(typeof&lt;UserId&gt;)&gt;] — skips only OverridablePreCondition&lt;UserId&gt;" ]

    /// GET /articles/{slug:alpha} — Custom path + constrained parser
    let article (slug: Slug) : HttpHandler =
        let (Slug s) = slug

        featurePage
            "Article"
            [ ("slug", s) ]
            [ "Custom path via [&lt;Route(Path = \"articles/{slug}\")&gt;]"
              "Extractor.constrainedParser adds :alpha constraint at endpoint registration"
              "ASP.NET Core rejects non-alpha slugs before handler runs" ]

    /// PUT /settings — Custom method + custom path + PreCondition
    let updateSettings (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        featurePage
            "Settings Updated"
            [ ("userId", string uid) ]
            [ "Custom method via [&lt;Route(RouteMethod.Put, Path = \"settings\")&gt;]"
              "PreCondition&lt;UserId&gt; — auth required" ]

    /// GET /dashboard — PreCondition<AdminId> requires Admin role
    let dashboard (adminId: AdminId) : HttpHandler =
        let (AdminId uid) = adminId

        featurePage
            "Admin Dashboard"
            [ ("adminId", string uid) ]
            [ "Path-less group via [&lt;Route(Path = \"\")&gt;] on parent"
              "PreCondition&lt;AdminId&gt; — requires Admin role" ]

    /// POST /contact — FormBody extraction
    let contactSubmit (input: ContactInput) : HttpHandler =
        featurePage
            "Contact Submitted"
            [ ("name", input.Name); ("message", input.Message) ]
            [ "FormBody&lt;ContactInput&gt; — request body deserialized from form data"
              "Create convention — POST method inferred from case name" ]

    /// GET /tag/{name:alpha:minlength(3):maxlength(50)} — Explicit attribute constraints
    let tag (name: string) : HttpHandler =
        featurePage
            "Tag"
            [ ("name", name) ]
            [ "[&lt;Route(Constraints = [| Alpha |], MinLength = 3, MaxLength = 50)&gt;]"
              "Multiple constraints combined: :alpha:minlength(3):maxlength(50)"
              "ASP.NET Core rejects non-alpha, too-short, or too-long values" ]

    // -------------------------------------------------------------------------
    // Login/Logout handlers (auth infrastructure, outside the Route union)
    // -------------------------------------------------------------------------

    /// GET /login — simple HTML form
    let loginPage: HttpHandler =
        layout
            "Log In"
            [ Elem.h1 [] [ Text.raw "Log In" ]
              Elem.form
                  [ Attr.method "post"; Attr.action "/login" ]
                  [ Elem.div
                        []
                        [ Elem.label [] [ Text.raw "User ID (GUID): " ]
                          Elem.input
                              [ Attr.type' "text"
                                Attr.name "userId"
                                Attr.value "11111111-1111-1111-1111-111111111111" ] ]
                    Elem.div
                        []
                        [ Elem.label
                              []
                              [ Elem.input [ Attr.type' "checkbox"; Attr.name "isAdmin"; Attr.value "true" ]
                                Text.raw " Admin" ] ]
                    Elem.div [] [ Elem.input [ Attr.type' "submit"; Attr.value "Log in" ] ] ]
              Elem.a [ Attr.href "/" ] [ Text.raw "Back to home" ] ]
        |> Response.ofHtml

    /// POST /login — creates ClaimsPrincipal and signs in with cookie auth
    let loginSubmit: HttpHandler =
        fun ctx ->
            task {
                let form = ctx.Request.Form
                let userId = form["userId"].ToString()
                let isAdmin = form["isAdmin"].ToString() = "true"

                let claims =
                    [ Claim(ClaimTypes.NameIdentifier, userId)
                      if isAdmin then
                          Claim(ClaimTypes.Role, "Admin") ]

                let identity =
                    ClaimsIdentity(claims, CookieAuthenticationDefaults.AuthenticationScheme)

                let principal = ClaimsPrincipal(identity)

                do! ctx.SignInAsync(CookieAuthenticationDefaults.AuthenticationScheme, principal)

                ctx.Response.Redirect("/")
            }
            :> _

    /// POST /logout — signs out and redirects to home
    let logoutSubmit: HttpHandler =
        fun ctx ->
            task {
                do! ctx.SignOutAsync(CookieAuthenticationDefaults.AuthenticationScheme)
                ctx.Response.Redirect("/")
            }
            :> _
