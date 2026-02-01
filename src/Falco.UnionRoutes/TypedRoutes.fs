namespace Falco.UnionRoutes

open System
open Falco
open Microsoft.AspNetCore.Http

/// Servant-inspired type-safe route handlers.
///
/// The key insight: union case fields define the parameter types.
/// This module provides compile-time verification that handlers
/// match their route's parameter structure.
///
/// Example:
/// ```fsharp
/// type PostRoute =
///     | List                       // () -> HttpHandler
///     | Detail of id: Guid         // Guid -> HttpHandler
///     | Update of id: Guid         // Guid -> HttpHandler (with auth)
///
/// let handlers =
///     typed<PostRoute, AppError> toErrorResponse {
///         route List Handlers.listPosts
///         route Detail (fun id -> Handlers.getPost (PostId id))
///         routeWith Update requireAuth (fun (user, id) -> Handlers.updatePost user (PostId id))
///     }
/// ```
[<AutoOpen>]
module TypedRoutes =

    // =========================================================================
    // Core Types
    // =========================================================================

    /// A typed route case with its handler.
    /// The 'Params type must match what the route case fields provide.
    type TypedCase<'Route, 'Params, 'Error> =
        {
            /// Pattern to match against
            Match: 'Route -> 'Params option
            /// Extract actual values from HTTP context
            Extract: HttpContext -> Result<'Params, 'Error>
            /// Handler that receives the extracted params
            Handle: 'Params -> HttpHandler
        }

    /// A route that requires no parameters (unit case)
    type UnitCase<'Route, 'Error> =
        { Match: 'Route -> bool
          Handle: HttpHandler }

    /// Builder state for collecting typed routes
    type TypedRouteBuilder<'Route, 'Error> =
        { ToErrorResponse: 'Error -> HttpHandler
          UnitCases: UnitCase<'Route, 'Error> list
          TypedCases: ('Route -> HttpHandler option) list }

    // =========================================================================
    // Extractors - derive from route field types
    // =========================================================================

    /// Extract a Guid from route parameters
    let extractGuid (paramName: string) (error: 'Error) (ctx: HttpContext) : Result<Guid, 'Error> =
        let route = Request.getRoute ctx
        let value = route.GetString paramName

        match Guid.TryParse(value) with
        | true, guid -> Ok guid
        | false, _ -> Error error

    /// Extract a string from route parameters
    let extractString (paramName: string) (error: 'Error) (ctx: HttpContext) : Result<string, 'Error> =
        let route = Request.getRoute ctx
        let value = route.GetString paramName

        if String.IsNullOrEmpty(value) then
            Error error
        else
            Ok value

    /// Extract an int from route parameters
    let extractInt (paramName: string) (error: 'Error) (ctx: HttpContext) : Result<int, 'Error> =
        let route = Request.getRoute ctx
        let value = route.GetString paramName

        match Int32.TryParse(value) with
        | true, i -> Ok i
        | false, _ -> Error error

    // =========================================================================
    // Builder Functions
    // =========================================================================

    /// Start building typed route handlers
    let typed<'Route, 'Error> (toErrorResponse: 'Error -> HttpHandler) : TypedRouteBuilder<'Route, 'Error> =
        { ToErrorResponse = toErrorResponse
          UnitCases = []
          TypedCases = [] }

    /// Add a route with no parameters
    let route0
        (pattern: 'Route -> bool)
        (handler: HttpHandler)
        (builder: TypedRouteBuilder<'Route, 'Error>)
        : TypedRouteBuilder<'Route, 'Error> =
        let case = { Match = pattern; Handle = handler }

        { builder with
            UnitCases = case :: builder.UnitCases }

    /// Add a route with one Guid parameter.
    /// The handler signature must accept Guid.
    let routeGuid
        (pattern: 'Route -> Guid option)
        (paramName: string)
        (error: 'Error)
        (handler: Guid -> HttpHandler)
        (builder: TypedRouteBuilder<'Route, 'Error>)
        : TypedRouteBuilder<'Route, 'Error> =
        let typedCase (route: 'Route) : HttpHandler option =
            match pattern route with
            | Some _ ->
                Some(fun ctx ->
                    task {
                        match extractGuid paramName error ctx with
                        | Ok guid -> return! handler guid ctx
                        | Error e -> return! builder.ToErrorResponse e ctx
                    })
            | None -> None

        { builder with
            TypedCases = typedCase :: builder.TypedCases }

    /// Add a route with one Guid parameter plus a pipeline for additional extraction.
    /// Useful for routes that need both route params AND auth/headers/etc.
    let routeGuidWith
        (pattern: 'Route -> Guid option)
        (paramName: string)
        (paramError: 'Error)
        (pipeline: Pipeline<'Extra, 'Error>)
        (handler: 'Extra * Guid -> HttpHandler)
        (builder: TypedRouteBuilder<'Route, 'Error>)
        : TypedRouteBuilder<'Route, 'Error> =
        let typedCase (route: 'Route) : HttpHandler option =
            match pattern route with
            | Some _ ->
                Some(fun ctx ->
                    task {
                        match extractGuid paramName paramError ctx, pipeline ctx with
                        | Ok guid, Ok extra -> return! handler (extra, guid) ctx
                        | Error e, _ -> return! builder.ToErrorResponse e ctx
                        | _, Error e -> return! builder.ToErrorResponse e ctx
                    })
            | None -> None

        { builder with
            TypedCases = typedCase :: builder.TypedCases }

    /// Add a route with one string parameter
    let routeString
        (pattern: 'Route -> string option)
        (paramName: string)
        (error: 'Error)
        (handler: string -> HttpHandler)
        (builder: TypedRouteBuilder<'Route, 'Error>)
        : TypedRouteBuilder<'Route, 'Error> =
        let typedCase (route: 'Route) : HttpHandler option =
            match pattern route with
            | Some _ ->
                Some(fun ctx ->
                    task {
                        match extractString paramName error ctx with
                        | Ok s -> return! handler s ctx
                        | Error e -> return! builder.ToErrorResponse e ctx
                    })
            | None -> None

        { builder with
            TypedCases = typedCase :: builder.TypedCases }

    /// Add a route with one int parameter
    let routeInt
        (pattern: 'Route -> int option)
        (paramName: string)
        (error: 'Error)
        (handler: int -> HttpHandler)
        (builder: TypedRouteBuilder<'Route, 'Error>)
        : TypedRouteBuilder<'Route, 'Error> =
        let typedCase (route: 'Route) : HttpHandler option =
            match pattern route with
            | Some _ ->
                Some(fun ctx ->
                    task {
                        match extractInt paramName error ctx with
                        | Ok i -> return! handler i ctx
                        | Error e -> return! builder.ToErrorResponse e ctx
                    })
            | None -> None

        { builder with
            TypedCases = typedCase :: builder.TypedCases }

    /// Build the final route handler function
    let build (builder: TypedRouteBuilder<'Route, 'Error>) : 'Route -> HttpHandler =
        fun route ->
            // Check unit cases first
            let unitMatch =
                builder.UnitCases
                |> List.tryFind (fun c -> c.Match route)
                |> Option.map (fun c -> c.Handle)

            match unitMatch with
            | Some handler -> handler
            | None ->
                // Check typed cases
                builder.TypedCases
                |> List.tryPick (fun f -> f route)
                |> Option.defaultWith (fun () ->
                    Response.withStatusCode 404 >> Response.ofPlainText "Route not configured")
