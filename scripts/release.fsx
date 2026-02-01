#!/usr/bin/env dotnet fsi

/// Release script with automatic semantic versioning based on API changes:
/// - MAJOR: removed or changed public APIs (breaking changes)
/// - MINOR: added public APIs (backward compatible)
/// - PATCH: no API changes (bug fixes, implementation changes)
///
/// Pre-release progression: alpha → beta → rc → stable

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open System.Reflection

let fsproj = "src/Falco.UnionRoutes/Falco.UnionRoutes.fsproj"
let dllPath = "src/Falco.UnionRoutes/bin/Release/net10.0/Falco.UnionRoutes.dll"
let apiBaselinePath = "api-baseline.txt"

let run (cmd: string) (args: string) =
    let psi = ProcessStartInfo(cmd, args)
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    use p = Process.Start(psi)
    let output = p.StandardOutput.ReadToEnd()
    let error = p.StandardError.ReadToEnd()
    p.WaitForExit()
    (p.ExitCode, output.Trim(), error.Trim())

let runOrFail cmd args =
    let (code, output, error) = run cmd args
    if code <> 0 then
        eprintfn "Command failed: %s %s" cmd args
        eprintfn "Error: %s" error
        exit 1
    output

let getLatestTag () =
    let (code, output, _) = run "git" "tag -l v* --sort=-v:refname"
    if code = 0 && output <> "" then
        output.Split('\n').[0]
    else
        ""

type PreRelease =
    | Alpha of int
    | Beta of int
    | RC of int
    | Stable

type Version = {
    Major: int
    Minor: int
    Patch: int
    PreRelease: PreRelease
}

let parseVersion (version: string) : Version =
    let v = version.TrimStart('v')
    let parts = v.Split('-')
    let baseParts = parts.[0].Split('.')
    let major = if baseParts.Length > 0 then int baseParts.[0] else 0
    let minor = if baseParts.Length > 1 then int baseParts.[1] else 0
    let patch = if baseParts.Length > 2 then int baseParts.[2] else 0

    let preRelease =
        if parts.Length > 1 then
            let pre = parts.[1]
            let numMatch = Regex.Match(pre, @"(\d+)$")
            let num = if numMatch.Success then int numMatch.Groups.[1].Value else 1
            if pre.StartsWith("alpha") then Alpha num
            elif pre.StartsWith("beta") then Beta num
            elif pre.StartsWith("rc") then RC num
            else Stable
        else
            Stable

    { Major = major; Minor = minor; Patch = patch; PreRelease = preRelease }

let formatVersion (v: Version) : string =
    let base' = sprintf "%d.%d.%d" v.Major v.Minor v.Patch
    match v.PreRelease with
    | Alpha n -> sprintf "%s-alpha.%d" base' n
    | Beta n -> sprintf "%s-beta.%d" base' n
    | RC n -> sprintf "%s-rc.%d" base' n
    | Stable -> base'

let isPreRelease (v: Version) =
    match v.PreRelease with
    | Stable -> false
    | _ -> true

// API extraction and comparison
let getPublicApi (dllPath: string) : string list =
    try
        let assembly = Assembly.LoadFrom(dllPath)
        assembly.GetExportedTypes()
        |> Array.collect (fun t ->
            let typeName = t.FullName
            let members =
                t.GetMembers(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
                |> Array.filter (fun m ->
                    match m.MemberType with
                    | MemberTypes.Constructor | MemberTypes.Method | MemberTypes.Property | MemberTypes.Field -> true
                    | _ -> false)
                |> Array.map (fun m ->
                    let memberSig =
                        match m with
                        | :? System.Reflection.MethodInfo as mi ->
                            let params' = mi.GetParameters() |> Array.map (fun p -> p.ParameterType.Name) |> String.concat ", "
                            sprintf "%s(%s): %s" mi.Name params' mi.ReturnType.Name
                        | :? System.Reflection.PropertyInfo as pi ->
                            sprintf "%s: %s" pi.Name pi.PropertyType.Name
                        | :? System.Reflection.FieldInfo as fi ->
                            sprintf "%s: %s" fi.Name fi.FieldType.Name
                        | :? System.Reflection.ConstructorInfo as ci ->
                            let params' = ci.GetParameters() |> Array.map (fun p -> p.ParameterType.Name) |> String.concat ", "
                            sprintf ".ctor(%s)" params'
                        | _ -> m.Name
                    sprintf "  %s" memberSig)
            [| sprintf "type %s" typeName |] |> Array.append members)
        |> Array.toList
        |> List.sort
    with ex ->
        eprintfn "Warning: Could not load assembly for API comparison: %s" ex.Message
        []

type ApiChange =
    | Breaking of removed: string list * changed: string list
    | Addition of added: string list
    | NoChange

let compareApis (baseline: string list) (current: string list) : ApiChange =
    let baselineSet = Set.ofList baseline
    let currentSet = Set.ofList current

    let removed = Set.difference baselineSet currentSet |> Set.toList
    let added = Set.difference currentSet baselineSet |> Set.toList

    if not (List.isEmpty removed) then
        Breaking(removed, [])
    elif not (List.isEmpty added) then
        Addition(added)
    else
        NoChange

let loadBaseline () : string list =
    if File.Exists(apiBaselinePath) then
        File.ReadAllLines(apiBaselinePath) |> Array.toList
    else
        []

let saveBaseline (api: string list) =
    File.WriteAllLines(apiBaselinePath, api)

// Version bumping based on API changes
let determineVersionBump (current: Version) (apiChange: ApiChange) : Version * string =
    match current.PreRelease with
    | Alpha n ->
        // In alpha, just bump alpha number regardless of changes
        { current with PreRelease = Alpha(n + 1) }, "alpha (API changes allowed in alpha)"
    | Beta n ->
        // In beta, breaking changes bump to next beta, additions are ok
        match apiChange with
        | Breaking _ -> { current with PreRelease = Beta(n + 1) }, "beta (breaking change)"
        | Addition _ -> { current with PreRelease = Beta(n + 1) }, "beta (new API)"
        | NoChange -> { current with PreRelease = Beta(n + 1) }, "beta"
    | RC n ->
        // In RC, any API change goes back to beta
        match apiChange with
        | Breaking _ -> { current with PreRelease = Beta 1 }, "back to beta (breaking change in RC)"
        | Addition _ -> { current with PreRelease = Beta 1 }, "back to beta (new API in RC)"
        | NoChange -> { current with PreRelease = RC(n + 1) }, "rc"
    | Stable ->
        match apiChange with
        | Breaking _ ->
            { Major = current.Major + 1; Minor = 0; Patch = 0; PreRelease = Stable }, "MAJOR (breaking API change)"
        | Addition _ ->
            { current with Minor = current.Minor + 1; Patch = 0 }, "MINOR (new API)"
        | NoChange ->
            { current with Patch = current.Patch + 1 }, "PATCH (no API changes)"

let hasUncommittedChanges () =
    let (code1, _, _) = run "git" "diff --quiet"
    let (code2, _, _) = run "git" "diff --cached --quiet"
    code1 <> 0 || code2 <> 0

let tagExists tag =
    let (_, output, _) = run "git" "tag -l"
    output.Split('\n') |> Array.contains tag

let updateVersionInFsproj newVersion =
    let content = File.ReadAllText(fsproj)
    let pattern = @"<Version>.*</Version>"
    let replacement = sprintf "<Version>%s</Version>" newVersion
    let newContent = Regex.Replace(content, pattern, replacement)
    File.WriteAllText(fsproj, newContent)

let promptYesNo message =
    printf "%s [y/N] " message
    let response = Console.ReadLine()
    response.ToLower() = "y"

let showHelp () =
    printfn "Usage: mise run release [command]"
    printfn ""
    printfn "Automatic versioning based on API changes:"
    printfn "  - Detects added/removed/changed public APIs"
    printfn "  - MAJOR bump for breaking changes (stable only)"
    printfn "  - MINOR bump for new APIs (stable only)"
    printfn "  - PATCH bump for no API changes (stable only)"
    printfn "  - Pre-release versions bump within their stage"
    printfn ""
    printfn "Commands:"
    printfn "  (none)    - auto-detect API changes and bump accordingly"
    printfn "  alpha     - start new alpha cycle for next minor version"
    printfn "  beta      - promote current version to beta"
    printfn "  rc        - promote current version to release candidate"
    printfn "  stable    - promote current version to stable"
    printfn "  init      - initialize API baseline from current build"
    printfn ""
    printfn "The API baseline is stored in %s" apiBaselinePath

[<EntryPoint>]
let main argv =
    let cmd = if argv.Length > 0 then argv.[0] else ""

    if cmd = "--help" || cmd = "-h" then
        showHelp ()
        exit 0

    // Build in Release mode to get the DLL
    printfn "Building in Release mode..."
    runOrFail "dotnet" "build -c Release" |> ignore

    if cmd = "init" then
        printfn "Initializing API baseline..."
        let api = getPublicApi (Path.GetFullPath dllPath)
        saveBaseline api
        printfn "Saved %d API entries to %s" api.Length apiBaselinePath
        printfn "Commit this file to track API changes."
        exit 0

    // Get current version
    let latestTag = getLatestTag ()
    let currentVersion =
        if String.IsNullOrEmpty(latestTag) then
            { Major = 0; Minor = 0; Patch = 0; PreRelease = Stable }
        else
            parseVersion latestTag

    let currentVersionStr = formatVersion currentVersion
    printfn "Current version: %s" currentVersionStr

    // Handle explicit promotion commands
    let newVersion, reason =
        match cmd with
        | "alpha" ->
            { currentVersion with Minor = currentVersion.Minor + 1; Patch = 0; PreRelease = Alpha 1 },
            "starting new alpha cycle"
        | "beta" ->
            { currentVersion with PreRelease = Beta 1 }, "promoting to beta"
        | "rc" ->
            { currentVersion with PreRelease = RC 1 }, "promoting to release candidate"
        | "stable" ->
            { currentVersion with PreRelease = Stable }, "promoting to stable"
        | "" ->
            // Auto-detect based on API changes
            let baseline = loadBaseline ()
            let currentApi = getPublicApi (Path.GetFullPath dllPath)

            if baseline.IsEmpty then
                printfn ""
                printfn "No API baseline found. Run 'mise run release init' first,"
                printfn "or use 'mise run release alpha' to start the first release."
                exit 1

            let apiChange = compareApis baseline currentApi

            match apiChange with
            | Breaking(removed, _) ->
                printfn ""
                printfn "Detected BREAKING API changes:"
                removed |> List.iter (printfn "  - %s")
            | Addition added ->
                printfn ""
                printfn "Detected new APIs:"
                added |> List.iter (printfn "  + %s")
            | NoChange ->
                printfn ""
                printfn "No API changes detected."

            determineVersionBump currentVersion apiChange
        | _ ->
            eprintfn "Unknown command: %s" cmd
            eprintfn "Run with --help for usage"
            exit 1

    let newVersionStr = formatVersion newVersion
    let newTag = sprintf "v%s" newVersionStr

    printfn ""
    printfn "Version bump: %s" reason
    printfn "New version: %s" newVersionStr
    printfn "New tag: %s" newTag

    if not (promptYesNo "Continue?") then
        printfn "Aborted."
        exit 0

    // Check for uncommitted changes (except the ones we're about to make)
    if hasUncommittedChanges () then
        eprintfn "Error: You have uncommitted changes. Please commit or stash them first."
        exit 1

    // Check if tag exists
    if tagExists newTag then
        eprintfn "Error: Tag %s already exists" newTag
        exit 1

    // Update API baseline
    let currentApi = getPublicApi (Path.GetFullPath dllPath)
    saveBaseline currentApi
    printfn "Updated API baseline"

    // Update version in .fsproj
    updateVersionInFsproj newVersionStr
    printfn "Updated %s to version %s" fsproj newVersionStr

    // Commit the version bump
    runOrFail "git" (sprintf "add %s %s" fsproj apiBaselinePath) |> ignore
    runOrFail "git" (sprintf "commit -m \"Release %s\"" newVersionStr) |> ignore
    printfn "Committed release"

    // Create tag
    runOrFail "git" (sprintf "tag -a %s -m \"Release %s\"" newTag newVersionStr) |> ignore
    printfn "Created tag %s" newTag

    // Prompt to push
    printfn ""
    printfn "Ready to push. This will trigger the release workflow."

    if promptYesNo "Push to origin?" then
        runOrFail "git" "push origin HEAD" |> ignore
        runOrFail "git" (sprintf "push origin %s" newTag) |> ignore
        printfn ""
        printfn "Pushed! Release workflow will run at:"
        printfn "https://github.com/michaelglass/Falco.UnionRoutes/actions"
    else
        printfn ""
        printfn "Not pushed. To push manually:"
        printfn "  git push origin HEAD"
        printfn "  git push origin %s" newTag

    0

main (fsi.CommandLineArgs |> Array.skip 1)
