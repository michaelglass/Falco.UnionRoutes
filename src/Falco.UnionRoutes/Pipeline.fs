namespace Falco.UnionRoutes

open System
open Falco
open Microsoft.AspNetCore.Http

/// Railway-oriented handler composition for Falco.
///
/// A Pipeline extracts and validates data from the HTTP context, short-circuiting
/// on the first error. Pipelines compose with the <&> operator.
///
/// Example:
/// ```fsharp
/// let requireAuth: Pipeline<UserId, MyError> = fun ctx ->
///     match getAuthFromCookie ctx with
///     | Some userId -> Ok userId
///     | None -> Error MyError.NotAuthenticated
///
/// let requireId name: Pipeline<Guid, MyError> = fun ctx ->
///     match tryGetRouteGuid ctx name with
///     | Some id -> Ok id
///     | None -> Error (MyError.BadRequest $"Invalid {name}")
///
/// // Compose pipelines
/// let handler =
///     Pipeline.run toErrorResponse (requireAuth <&> requireId "id") (fun (userId, id) ->
///         // Handle request with validated userId and id
///         Response.ofJson {| userId = userId; id = id |})
/// ```
[<AutoOpen>]
module Pipeline =

    /// A pipeline extracts/validates something from the HTTP context.
    /// On failure, returns an error of type 'TError.
    type Pipeline<'T, 'TError> = HttpContext -> Result<'T, 'TError>

    // =========================================================================
    // Result Helpers
    // =========================================================================

    /// Convert Option to Result with specified error
    let requireSome error opt =
        match opt with
        | Some v -> Ok v
        | None -> Error error

    /// Convert Option to Result, applying a function to create the error
    let requireSomeWith errorFn opt =
        match opt with
        | Some v -> Ok v
        | None -> Error(errorFn ())

    // =========================================================================
    // Pipeline Composition
    // =========================================================================

    /// Compose two pipelines, returning a tuple of results.
    /// Short-circuits on the first error.
    let (<&>) (p1: Pipeline<'a, 'e>) (p2: Pipeline<'b, 'e>) : Pipeline<'a * 'b, 'e> =
        fun ctx ->
            match p1 ctx with
            | Error e -> Error e
            | Ok a ->
                match p2 ctx with
                | Error e -> Error e
                | Ok b -> Ok(a, b)

    /// Map over a pipeline result
    let map (f: 'a -> 'b) (p: Pipeline<'a, 'e>) : Pipeline<'b, 'e> = fun ctx -> p ctx |> Result.map f

    /// Ignore pipeline result (useful for validation-only checks)
    let ignoreResult (p: Pipeline<'a, 'e>) : Pipeline<unit, 'e> = fun ctx -> p ctx |> Result.map ignore

    /// Bind two pipelines (flatMap)
    let bind (f: 'a -> Pipeline<'b, 'e>) (p: Pipeline<'a, 'e>) : Pipeline<'b, 'e> =
        fun ctx ->
            match p ctx with
            | Error e -> Error e
            | Ok a -> f a ctx

    /// Create a pipeline that always succeeds with a value
    let succeed (value: 'a) : Pipeline<'a, 'e> = fun _ -> Ok value

    /// Create a pipeline that always fails with an error
    let fail (error: 'e) : Pipeline<'a, 'e> = fun _ -> Error error

    // =========================================================================
    // Route Parameter Extraction
    // =========================================================================

    /// Try to parse a GUID from a route parameter
    let tryGetRouteGuid (ctx: HttpContext) (paramName: string) : Guid option =
        let route = Request.getRoute ctx
        let idStr = route.GetString paramName

        match Guid.TryParse(idStr) with
        | true, guid -> Some guid
        | false, _ -> None

    /// Require a GUID route parameter, mapping to a typed ID.
    ///
    /// Example:
    /// ```fsharp
    /// type UserId = UserId of Guid
    /// let requireUserId = requireRouteId "id" UserId (MyError.BadRequest "Invalid user ID")
    /// ```
    let requireRouteId<'TId, 'TError>
        (paramName: string)
        (constructor: Guid -> 'TId)
        (error: 'TError)
        : Pipeline<'TId, 'TError> =
        fun ctx -> tryGetRouteGuid ctx paramName |> Option.map constructor |> requireSome error

    /// Require a GUID route parameter with dynamic error message.
    let requireRouteIdWith<'TId, 'TError>
        (paramName: string)
        (constructor: Guid -> 'TId)
        (errorFn: unit -> 'TError)
        : Pipeline<'TId, 'TError> =
        fun ctx ->
            tryGetRouteGuid ctx paramName
            |> Option.map constructor
            |> requireSomeWith errorFn

    /// Require a string route parameter.
    let requireRouteStr<'TError> (paramName: string) (error: 'TError) : Pipeline<string, 'TError> =
        fun ctx ->
            let route = Request.getRoute ctx
            let value = route.GetString paramName

            if String.IsNullOrEmpty(value) then
                Error error
            else
                Ok value

    /// Require a string route parameter with dynamic error.
    let requireRouteStrWith<'TError> (paramName: string) (errorFn: unit -> 'TError) : Pipeline<string, 'TError> =
        fun ctx ->
            let route = Request.getRoute ctx
            let value = route.GetString paramName

            if String.IsNullOrEmpty(value) then
                Error(errorFn ())
            else
                Ok value

    /// Require an int route parameter.
    let requireRouteInt<'TError> (paramName: string) (error: 'TError) : Pipeline<int, 'TError> =
        fun ctx ->
            let route = Request.getRoute ctx
            let value = route.GetString paramName

            match Int32.TryParse(value) with
            | true, i -> Ok i
            | false, _ -> Error error

    // =========================================================================
    // Pipeline Execution
    // =========================================================================

    /// Run a pipeline, converting errors to HTTP responses.
    ///
    /// This is the main entry point for executing a pipeline.
    ///
    /// Example:
    /// ```fsharp
    /// let toResponse = function
    ///     | MyError.NotAuthenticated -> Response.withStatusCode 401 >> Response.ofPlainText "Unauthorized"
    ///     | MyError.BadRequest msg -> Response.withStatusCode 400 >> Response.ofPlainText msg
    ///     | MyError.NotFound msg -> Response.withStatusCode 404 >> Response.ofPlainText msg
    ///
    /// let handler = run toResponse requireAuth (fun userId -> Response.ofJson {| id = userId |})
    /// ```
    let run
        (toResponse: 'TError -> HttpHandler)
        (pipeline: Pipeline<'a, 'TError>)
        (handler: 'a -> HttpHandler)
        : HttpHandler =
        fun ctx ->
            task {
                match pipeline ctx with
                | Error e -> return! toResponse e ctx
                | Ok a -> return! handler a ctx
            }

    /// Run a pipeline with different error handlers for different contexts.
    /// Useful when the same pipeline should redirect for pages but return JSON for APIs.
    let runWith
        (toResponse: 'TError -> HttpHandler)
        (pipeline: Pipeline<'a, 'TError>)
        (handler: 'a -> HttpHandler)
        : HttpHandler =
        run toResponse pipeline handler
