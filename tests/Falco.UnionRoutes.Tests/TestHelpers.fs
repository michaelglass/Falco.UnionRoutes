module Falco.UnionRoutes.Tests.TestHelpers

open System
open Falco
open Microsoft.AspNetCore.Http
open Falco.UnionRoutes

// =========================================================================
// Option/Result Helpers
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
// Extractor Composition
// =========================================================================

/// Composes two extractors, returning a tuple of both results.
/// Short-circuits on the first error.
let (<&>) (e1: Extractor<'a, 'e>) (e2: Extractor<'b, 'e>) : Extractor<'a * 'b, 'e> =
    fun ctx ->
        match e1 ctx with
        | Error e -> Error e
        | Ok a ->
            match e2 ctx with
            | Error e -> Error e
            | Ok b -> Ok(a, b)

// =========================================================================
// Route Parameter Helpers
// =========================================================================

/// Try to parse a GUID from a route parameter
let tryGetRouteGuid (ctx: HttpContext) (paramName: string) : Guid option =
    let route = Request.getRoute ctx
    let idStr = route.GetString paramName

    match Guid.TryParse(idStr) with
    | true, guid -> Some guid
    | false, _ -> None

/// Require a GUID route parameter, mapping to a typed ID.
let requireRouteId<'TId, 'E> (paramName: string) (constructor: Guid -> 'TId) (error: 'E) : Extractor<'TId, 'E> =
    fun ctx -> tryGetRouteGuid ctx paramName |> Option.map constructor |> requireSome error

/// Require a string route parameter.
let requireRouteStr<'E> (paramName: string) (error: 'E) : Extractor<string, 'E> =
    fun ctx ->
        let route = Request.getRoute ctx
        let value = route.GetString paramName

        if String.IsNullOrEmpty(value) then
            Error error
        else
            Ok value

/// Require an int route parameter.
let requireRouteInt<'E> (paramName: string) (error: 'E) : Extractor<int, 'E> =
    fun ctx ->
        let route = Request.getRoute ctx
        let value = route.GetString paramName

        match Int32.TryParse(value) with
        | true, i -> Ok i
        | false, _ -> Error error

/// Require an int route parameter with dynamic error.
let requireRouteIntWith<'E> (paramName: string) (errorFn: unit -> 'E) : Extractor<int, 'E> =
    fun ctx ->
        let route = Request.getRoute ctx
        let value = route.GetString paramName

        match Int32.TryParse(value) with
        | true, i -> Ok i
        | false, _ -> Error(errorFn ())

/// Ignore extractor result (useful for validation-only checks)
let ignoreResult (e: Extractor<'a, 'e>) : Extractor<unit, 'e> = fun ctx -> e ctx |> Result.map ignore
