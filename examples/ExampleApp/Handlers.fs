namespace ExampleApp

open System
open Falco
open Falco.Markup
open Falco.UnionRoutes

// =============================================================================
// Domain Types
// =============================================================================

type UserId = UserId of Guid
type Slug = Slug of string

type AppError =
    | NotAuthenticated
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
                      Elem.p [] [ Text.raw "X-User-Id header required." ]
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
    // Auth extractor
    // -------------------------------------------------------------------------

    /// Extracts UserId from X-User-Id header.
    let requireAuth: Extractor<UserId, AppError> =
        fun ctx ->
            match ctx.Request.Headers.TryGetValue("X-User-Id") with
            | true, values ->
                match Guid.TryParse(values.ToString()) with
                | true, guid -> Ok(UserId guid)
                | false, _ -> Error NotAuthenticated
            | false, _ -> Error NotAuthenticated

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

    /// POST /posts — Create/POST convention + PreCondition
    let postCreate (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        featurePage
            "Post Created"
            [ ("userId", string uid) ]
            [ "Create convention — POST method inferred from case name"
              "PreCondition&lt;UserId&gt; — value extracted via precondition" ]

    /// GET /posts/search — Default kebab-case + required QueryParam
    let postSearch (query: QueryParam<string>) : HttpHandler =
        let (QueryParam q) = query

        featurePage
            "Post Search"
            [ ("query", q) ]
            [ "Default kebab-case path — Search becomes /search"
              "QueryParam&lt;string&gt; — required query parameter" ]

    /// GET /posts/{id} — Show convention
    let postShow (id: Guid) : HttpHandler =
        featurePage
            "Post Detail"
            [ ("id", string id) ]
            [ "Show convention — param-only path, no case-name segment"
              "Built-in Guid extraction from route" ]

    /// GET /posts/{id}/edit — Edit convention
    let postEdit (id: Guid) : HttpHandler =
        featurePage "Post Edit" [ ("id", string id) ] [ "Edit convention — produces /edit path segment" ]

    /// DELETE /posts/{id} — Delete convention
    let postDelete (id: Guid) : HttpHandler =
        featurePage "Post Deleted" [ ("id", string id) ] [ "Delete convention — DELETE method inferred from case name" ]

    /// PATCH /posts/{id} — Patch convention
    let postPatch (id: Guid) : HttpHandler =
        featurePage "Post Patched" [ ("id", string id) ] [ "Patch convention — PATCH method inferred from case name" ]

    /// GET /items/{userId} — inherits OverridablePreCondition
    let itemList (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        featurePage
            "Item List"
            [ ("userId", string uid) ]
            [ "OverridablePreCondition&lt;UserId&gt; — precondition inherited from parent"
              "Single-case DU wrapper — UserId of Guid in path" ]

    /// GET /items/{userId}/public — SkipAllPreconditions
    let itemPublic (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        featurePage
            "Public Items"
            [ ("userId", string uid) ]
            [ "[&lt;SkipAllPreconditions&gt;] — all OverridablePreCondition skipped"
              "No X-User-Id header required for this route" ]

    /// GET /items/{userId}/limited — SkipPrecondition(typeof<UserId>)
    let itemLimited (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        featurePage
            "Limited Items"
            [ ("userId", string uid) ]
            [ "[&lt;SkipPrecondition(typeof&lt;UserId&gt;)&gt;] — skips only OverridablePreCondition&lt;UserId&gt;" ]

    /// GET /articles/{slug} — Custom path + custom parser
    let article (slug: Slug) : HttpHandler =
        let (Slug s) = slug

        featurePage
            "Article"
            [ ("slug", s) ]
            [ "Custom path via [&lt;Route(Path = \"articles/{slug}\")&gt;]"
              "Custom parser via Extractor.parser" ]

    /// PUT /settings — Custom method + custom path + PreCondition
    let updateSettings (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        featurePage
            "Settings Updated"
            [ ("userId", string uid) ]
            [ "Custom method via [&lt;Route(RouteMethod.Put, Path = \"settings\")&gt;]"
              "PreCondition&lt;UserId&gt; — auth required" ]

    /// GET /dashboard — Path-less group + PreCondition
    let dashboard (userId: UserId) : HttpHandler =
        let (UserId uid) = userId

        featurePage
            "Admin Dashboard"
            [ ("userId", string uid) ]
            [ "Path-less group via [&lt;Route(Path = \"\")&gt;] on parent"
              "PreCondition&lt;UserId&gt; — auth required" ]

    /// GET /health — Default kebab-case
    let health: HttpHandler = Response.ofJson {| status = "ok" |}
