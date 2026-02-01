#!/usr/bin/env dotnet fsi

/// Release script with automatic semantic versioning based on API changes:
/// - MAJOR: removed or changed public APIs (breaking changes)
/// - MINOR: added public APIs (backward compatible)
/// - PATCH: no API changes (bug fixes, implementation changes)
///
/// Pre-release progression: alpha → beta → rc → stable
///
/// API baseline is automatically extracted from the previous release tag.

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open System.Reflection

let fsproj = "src/Falco.UnionRoutes/Falco.UnionRoutes.fsproj"
let dllRelativePath = "src/Falco.UnionRoutes/bin/Release/net10.0/Falco.UnionRoutes.dll"

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

let runSilent cmd args =
    let (code, output, _) = run cmd args
    if code = 0 then Some output else None

let getLatestTag () =
    let (code, output, _) = run "git" "tag -l v* --sort=-v:refname"
    if code = 0 && output <> "" then
        Some (output.Split('\n').[0])
    else
        None

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

// API extraction via reflection
let getPublicApi (dllPath: string) : string list =
    try
        // Load assembly in reflection-only context to avoid locking
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
                        | :? MethodInfo as mi when not (mi.IsSpecialName) ->
                            let params' = mi.GetParameters() |> Array.map (fun p -> p.ParameterType.Name) |> String.concat ", "
                            Some (sprintf "  %s(%s): %s" mi.Name params' mi.ReturnType.Name)
                        | :? PropertyInfo as pi ->
                            Some (sprintf "  %s: %s" pi.Name pi.PropertyType.Name)
                        | :? FieldInfo as fi when fi.IsPublic ->
                            Some (sprintf "  %s: %s" fi.Name fi.FieldType.Name)
                        | :? ConstructorInfo as ci ->
                            let params' = ci.GetParameters() |> Array.map (fun p -> p.ParameterType.Name) |> String.concat ", "
                            Some (sprintf "  .ctor(%s)" params')
                        | _ -> None
                    memberSig)
                |> Array.choose id
            Array.append [| sprintf "type %s" typeName |] members)
        |> Array.toList
        |> List.sort
    with ex ->
        eprintfn "Warning: Could not load assembly: %s" ex.Message
        []

// Get API from a specific git tag by checking it out temporarily
let getApiFromTag (tag: string) : string list =
    let currentBranch = runOrFail "git" "rev-parse --abbrev-ref HEAD"
    let currentCommit = runOrFail "git" "rev-parse HEAD"

    try
        // Stash any changes (shouldn't be any, but just in case)
        let _ = runSilent "git" "stash"

        // Checkout the tag
        printfn "  Checking out %s..." tag
        runOrFail "git" (sprintf "checkout %s --quiet" tag) |> ignore

        // Build at that tag
        printfn "  Building at %s..." tag
        runOrFail "dotnet" "build -c Release --verbosity quiet" |> ignore

        // Extract API
        let api = getPublicApi (Path.GetFullPath dllRelativePath)

        // Return to original state
        printfn "  Returning to current state..."
        runOrFail "git" (sprintf "checkout %s --quiet" currentCommit) |> ignore
        let _ = runSilent "git" "stash pop"

        api
    with ex ->
        // Try to recover
        let _ = runSilent "git" (sprintf "checkout %s --quiet" currentCommit)
        let _ = runSilent "git" "stash pop"
        eprintfn "Error getting API from tag: %s" ex.Message
        []

type ApiChange =
    | Breaking of removed: string list
    | Addition of added: string list
    | NoChange

let compareApis (baseline: string list) (current: string list) : ApiChange =
    let baselineSet = Set.ofList baseline
    let currentSet = Set.ofList current

    let removed = Set.difference baselineSet currentSet |> Set.toList
    let added = Set.difference currentSet baselineSet |> Set.toList

    if not (List.isEmpty removed) then
        Breaking removed
    elif not (List.isEmpty added) then
        Addition added
    else
        NoChange

// Version bumping based on API changes
let determineVersionBump (current: Version) (apiChange: ApiChange) : Version * string =
    match current.PreRelease with
    | Alpha n ->
        { current with PreRelease = Alpha(n + 1) }, "alpha (API changes allowed)"
    | Beta n ->
        { current with PreRelease = Beta(n + 1) }, "beta"
    | RC n ->
        match apiChange with
        | Breaking _ -> { current with PreRelease = Beta 1 }, "back to beta (breaking change in RC!)"
        | Addition _ -> { current with PreRelease = Beta 1 }, "back to beta (new API in RC)"
        | NoChange -> { current with PreRelease = RC(n + 1) }, "rc"
    | Stable ->
        match apiChange with
        | Breaking _ ->
            { Major = current.Major + 1; Minor = 0; Patch = 0; PreRelease = Stable }, "MAJOR (breaking change)"
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
    printfn "  - Compares current API against the previous release tag"
    printfn "  - MAJOR bump for breaking changes (stable only)"
    printfn "  - MINOR bump for new APIs (stable only)"
    printfn "  - PATCH bump for no API changes (stable only)"
    printfn "  - Pre-release versions bump within their stage"
    printfn ""
    printfn "Commands:"
    printfn "  (none)    - auto-detect API changes and bump accordingly"
    printfn "  alpha     - start first alpha (0.1.0-alpha.1) or new cycle"
    printfn "  beta      - promote to beta"
    printfn "  rc        - promote to release candidate"
    printfn "  stable    - promote to stable"

[<EntryPoint>]
let main argv =
    let cmd = if argv.Length > 0 then argv.[0] else ""

    if cmd = "--help" || cmd = "-h" then
        showHelp ()
        exit 0

    // Check for uncommitted changes first
    if hasUncommittedChanges () then
        eprintfn "Error: You have uncommitted changes. Please commit or stash them first."
        exit 1

    // Get current version from latest tag
    let latestTag = getLatestTag ()
    let currentVersion =
        match latestTag with
        | Some tag -> parseVersion tag
        | None -> { Major = 0; Minor = 0; Patch = 0; PreRelease = Stable }

    let currentVersionStr = formatVersion currentVersion
    printfn "Current version: %s" (if latestTag.IsSome then currentVersionStr else "(none)")

    // Build current version
    printfn "Building current version..."
    runOrFail "dotnet" "build -c Release --verbosity quiet" |> ignore
    let currentApi = getPublicApi (Path.GetFullPath dllRelativePath)

    // Handle commands
    let newVersion, reason =
        match cmd with
        | "alpha" ->
            match latestTag with
            | None ->
                // First release ever
                { Major = 0; Minor = 1; Patch = 0; PreRelease = Alpha 1 }, "first alpha release"
            | Some _ ->
                // Start new alpha cycle
                { currentVersion with Minor = currentVersion.Minor + 1; Patch = 0; PreRelease = Alpha 1 },
                "starting new alpha cycle"
        | "beta" ->
            { currentVersion with PreRelease = Beta 1 }, "promoting to beta"
        | "rc" ->
            { currentVersion with PreRelease = RC 1 }, "promoting to release candidate"
        | "stable" ->
            { currentVersion with PreRelease = Stable }, "promoting to stable"
        | "" ->
            match latestTag with
            | None ->
                // No previous release - start with alpha
                printfn ""
                printfn "No previous releases found. Use 'mise run release alpha' for first release."
                exit 0
            | Some tag ->
                // Get API from previous release
                printfn ""
                printfn "Getting API baseline from %s..." tag
                let baselineApi = getApiFromTag tag

                if baselineApi.IsEmpty then
                    printfn "Warning: Could not extract API from previous release."
                    printfn "Proceeding with pre-release bump..."
                    determineVersionBump currentVersion NoChange
                else
                    let apiChange = compareApis baselineApi currentApi

                    match apiChange with
                    | Breaking removed ->
                        printfn ""
                        printfn "BREAKING API changes detected:"
                        removed |> List.take (min 10 removed.Length) |> List.iter (printfn "  - %s")
                        if removed.Length > 10 then printfn "  ... and %d more" (removed.Length - 10)
                    | Addition added ->
                        printfn ""
                        printfn "New APIs detected:"
                        added |> List.take (min 10 added.Length) |> List.iter (printfn "  + %s")
                        if added.Length > 10 then printfn "  ... and %d more" (added.Length - 10)
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

    // Check if tag exists
    if tagExists newTag then
        eprintfn "Error: Tag %s already exists" newTag
        exit 1

    if not (promptYesNo "Continue?") then
        printfn "Aborted."
        exit 0

    // Update version in .fsproj
    updateVersionInFsproj newVersionStr
    printfn "Updated %s" fsproj

    // Commit and tag
    runOrFail "git" (sprintf "add %s" fsproj) |> ignore
    runOrFail "git" (sprintf "commit -m \"Release %s\"" newVersionStr) |> ignore
    runOrFail "git" (sprintf "tag -a %s -m \"Release %s\"" newTag newVersionStr) |> ignore
    printfn "Created commit and tag %s" newTag

    // Push
    printfn ""
    if promptYesNo "Push to trigger release?" then
        runOrFail "git" "push origin HEAD" |> ignore
        runOrFail "git" (sprintf "push origin %s" newTag) |> ignore
        printfn ""
        printfn "Pushed! Release workflow: https://github.com/michaelglass/Falco.UnionRoutes/actions"
    else
        printfn ""
        printfn "To push manually: git push origin HEAD && git push origin %s" newTag

    0

main (fsi.CommandLineArgs |> Array.skip 1)
