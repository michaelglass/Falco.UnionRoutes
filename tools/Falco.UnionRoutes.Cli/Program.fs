open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.Loader
open System.Text.Json
open Falco.UnionRoutes
open Microsoft.FSharp.Reflection

// =============================================================================
// Arg parsing
// =============================================================================

type CliArgs =
    { FsprojPath: string
      RouteTypeName: string option
      Title: string option
      Version: string
      Description: string option
      OutputPath: string option
      NoBuild: bool }

let printUsage () =
    eprintfn "Usage: falco-routes <fsproj-path> [route-type-name] [options]"
    eprintfn ""
    eprintfn "Arguments:"
    eprintfn "  <fsproj-path>       Path to .fsproj file"
    eprintfn "  [route-type-name]   Fully-qualified route type (auto-detected if unique)"
    eprintfn ""
    eprintfn "Options:"
    eprintfn "  --title <title>       API title (default: assembly name)"
    eprintfn "  --version <version>   API version (default: \"1.0.0\")"
    eprintfn "  --description <desc>  Optional API description"
    eprintfn "  --output <path>       Output file (default: stdout)"
    eprintfn "  --no-build            Skip building the project"

let parseArgs (argv: string array) =
    if argv.Length = 0 then
        printUsage ()
        exit 1

    let mutable fsprojPath = None
    let mutable routeTypeName = None
    let mutable title = None
    let mutable version = "1.0.0"
    let mutable description = None
    let mutable outputPath = None
    let mutable noBuild = false
    let mutable i = 0

    while i < argv.Length do
        match argv.[i] with
        | "--title" when i + 1 < argv.Length ->
            i <- i + 1
            title <- Some argv.[i]
        | "--version" when i + 1 < argv.Length ->
            i <- i + 1
            version <- argv.[i]
        | "--description" when i + 1 < argv.Length ->
            i <- i + 1
            description <- Some argv.[i]
        | "--output" when i + 1 < argv.Length ->
            i <- i + 1
            outputPath <- Some argv.[i]
        | "--no-build" -> noBuild <- true
        | "--help"
        | "-h" ->
            printUsage ()
            exit 0
        | arg when arg.StartsWith("--") ->
            eprintfn "Unknown option: %s" arg
            exit 1
        | arg ->
            if fsprojPath.IsNone then
                fsprojPath <- Some arg
            elif routeTypeName.IsNone then
                routeTypeName <- Some arg
            else
                eprintfn "Unexpected argument: %s" arg
                exit 1

        i <- i + 1

    match fsprojPath with
    | None ->
        eprintfn "Error: fsproj path is required"
        exit 1
    | Some path ->
        { FsprojPath = path
          RouteTypeName = routeTypeName
          Title = title
          Version = version
          Description = description
          OutputPath = outputPath
          NoBuild = noBuild }

// =============================================================================
// Process execution
// =============================================================================

let runProcess (fileName: string) (args: string) =
    let psi = ProcessStartInfo(fileName, args)
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    let p = Process.Start(psi)
    let stdout = p.StandardOutput.ReadToEnd()
    let stderr = p.StandardError.ReadToEnd()
    p.WaitForExit()
    struct (p.ExitCode, stdout, stderr)

// =============================================================================
// Build project and find DLL
// =============================================================================

let buildProject (fsprojPath: string) =
    eprintfn "Building %s..." (Path.GetFileName fsprojPath)

    let struct (exitCode, stdout, stderr) =
        runProcess "dotnet" $"build \"{fsprojPath}\" -v:q -nologo"

    if exitCode <> 0 then
        eprintfn "Build failed:"

        if stdout.Length > 0 then
            eprintfn "%s" stdout

        if stderr.Length > 0 then
            eprintfn "%s" stderr

        exit 1

let getDllPath (fsprojPath: string) =
    let struct (exitCode, stdout, _) =
        runProcess "dotnet" $"msbuild \"{fsprojPath}\" -getProperty:TargetPath -nologo"

    if exitCode <> 0 then
        eprintfn "Error: Failed to determine output path for %s" fsprojPath
        exit 1

    let targetPath =
        try
            let doc = JsonDocument.Parse(stdout)
            doc.RootElement.GetProperty("Properties").GetProperty("TargetPath").GetString()
        with _ ->
            stdout.Trim()

    let projectDir = Path.GetDirectoryName(Path.GetFullPath fsprojPath)
    let dllPath = Path.GetFullPath(targetPath, projectDir)

    if not (File.Exists dllPath) then
        eprintfn "Error: output assembly not found at %s" dllPath
        eprintfn "Make sure the project has been built."
        exit 1

    dllPath

// =============================================================================
// Assembly loading
// =============================================================================

type SpecGenLoadContext(resolver: AssemblyDependencyResolver) =
    inherit AssemblyLoadContext("spec-gen")

    override _.Load(name) =
        // Share FSharp.Core and Falco.UnionRoutes with the CLI tool's context
        // so type identity is preserved for marker types and Spec.generate
        match name.Name with
        | "FSharp.Core"
        | "Falco.UnionRoutes" -> null // fall back to default context
        | _ ->
            match resolver.ResolveAssemblyToPath(name) with
            | null -> null
            | path -> base.LoadFromAssemblyPath(path)

let loadAssembly (dllPath: string) =
    let resolver = AssemblyDependencyResolver(dllPath)
    let ctx = SpecGenLoadContext(resolver)
    ctx.LoadFromAssemblyPath(dllPath)

// =============================================================================
// Route type auto-detection
// =============================================================================

let private isUnionType (t: Type) =
    try
        FSharpType.IsUnion(t, true)
    with _ ->
        false

let private markerTypeDefs =
    System.Collections.Generic.HashSet<Type>(
        [| typedefof<QueryParam<_>>
           typedefof<PreCondition<_>>
           typedefof<OverridablePreCondition<_>>
           typedefof<JsonBody<_>>
           typedefof<FormBody<_>>
           typedefof<Returns<_>> |]
    )

let private isMarkerType (t: Type) =
    t.IsGenericType && markerTypeDefs.Contains(t.GetGenericTypeDefinition())

/// Filter out compiler-generated union case subtypes (e.g. Route+Posts).
/// These are nested inside their parent union type and are not standalone DU types.
let private isTopLevelUnion (t: Type) =
    isUnionType t && (not t.IsNested || not (isUnionType t.DeclaringType))

let findRouteTypes (asm: Assembly) =
    let visited = System.Collections.Generic.HashSet<Type>()

    let rec hasRouteMarkers (t: Type) =
        if not (visited.Add t) then
            false
        elif not (isTopLevelUnion t) then
            false
        else
            FSharpType.GetUnionCases(t, true)
            |> Array.exists (fun case ->
                case.GetCustomAttributes(typeof<RouteAttribute>) |> Array.isEmpty |> not
                || case.GetFields()
                   |> Array.exists (fun f -> isMarkerType f.PropertyType || hasRouteMarkers f.PropertyType))

    let routeTypes = asm.GetExportedTypes() |> Array.filter hasRouteMarkers

    // Find types used as nested routes (fields in other route types)
    let nestedTypes =
        let hs = System.Collections.Generic.HashSet<Type>()

        for t in routeTypes do
            for c in FSharpType.GetUnionCases(t, true) do
                for f in c.GetFields() do
                    if routeTypes |> Array.contains f.PropertyType then
                        hs.Add(f.PropertyType) |> ignore

        hs

    // Root types: route types that aren't nested in other route types
    routeTypes |> Array.filter (fun t -> not (nestedTypes.Contains t))

let resolveRouteType (asm: Assembly) (explicitName: string option) =
    match explicitName with
    | Some name ->
        match asm.GetType(name) with
        | null ->
            eprintfn "Error: type '%s' not found in assembly %s" name (asm.GetName().Name)

            let candidates =
                asm.GetExportedTypes()
                |> Array.filter isTopLevelUnion
                |> Array.map (fun t -> t.FullName)

            if candidates.Length > 0 then
                eprintfn "Available union types:"

                for c in candidates do
                    eprintfn "  %s" c

            exit 1
        | t ->
            if not (isUnionType t) then
                eprintfn "Error: '%s' is not a discriminated union type" name
                exit 1

            t
    | None ->
        let roots = findRouteTypes asm

        match roots.Length with
        | 0 ->
            eprintfn "Error: no route types found in assembly %s" (asm.GetName().Name)

            eprintfn "A route type is a discriminated union that uses Falco.UnionRoutes marker types"
            eprintfn "(QueryParam, PreCondition, JsonBody, etc.) or the [<Route>] attribute."
            exit 1
        | 1 ->
            eprintfn "Auto-detected route type: %s" roots.[0].FullName
            roots.[0]
        | _ ->
            eprintfn "Error: multiple route types found in assembly %s" (asm.GetName().Name)

            eprintfn "Specify which one to use as the second argument:"

            for t in roots do
                eprintfn "  falco-routes %s %s" "<fsproj>" t.FullName

            exit 1

// =============================================================================
// Spec generation via reflection
// =============================================================================

let generateSpec (routeType: Type) (title: string) (version: string) (description: string option) =
    let config: OpenApiConfig =
        { Title = title
          Version = version
          Description = description }

    let specModule = typeof<OpenApiConfig>.Assembly.GetType("Falco.UnionRoutes.Spec")

    let generateMethod = specModule.GetMethod("generate").MakeGenericMethod(routeType)

    generateMethod.Invoke(null, [| box config |]) :?> string

// =============================================================================
// Entry point
// =============================================================================

[<EntryPoint>]
let main argv =
    let args = parseArgs argv
    let fsprojPath = Path.GetFullPath args.FsprojPath

    if not (File.Exists fsprojPath) then
        eprintfn "Error: file not found: %s" fsprojPath
        exit 1

    if not (fsprojPath.EndsWith(".fsproj")) then
        eprintfn "Error: expected an .fsproj file, got: %s" fsprojPath
        exit 1

    // Build unless --no-build
    if not args.NoBuild then
        buildProject fsprojPath

    // Find and load the output DLL
    let dllPath = getDllPath fsprojPath
    let asm = loadAssembly dllPath

    // Resolve route type
    let routeType = resolveRouteType asm args.RouteTypeName

    // Generate spec
    let title = args.Title |> Option.defaultValue (asm.GetName().Name)

    let json =
        try
            generateSpec routeType title args.Version args.Description
        with ex ->
            eprintfn "Error generating spec: %s" ex.Message
            exit 1

    // Output
    match args.OutputPath with
    | Some path ->
        File.WriteAllText(path, json)
        eprintfn "Wrote OpenAPI spec to %s" path
    | None -> printfn "%s" json

    0
