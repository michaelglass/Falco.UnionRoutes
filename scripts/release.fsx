#!/usr/bin/env dotnet fsi

/// Release script with automatic semantic versioning based on API changes.
/// API baseline is automatically extracted from the previous release tag.

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

// ============================================================================
// Configuration
// ============================================================================

let fsproj = "src/Falco.UnionRoutes/Falco.UnionRoutes.fsproj"
let dllPath = "src/Falco.UnionRoutes/bin/Release/net10.0/Falco.UnionRoutes.dll"
let repoUrl = "https://github.com/michaelglass/Falco.UnionRoutes"

// ============================================================================
// Domain Types
// ============================================================================

type PreRelease =
    | Alpha of int
    | Beta of int
    | RC of int

type VersionStage =
    | PreRelease of PreRelease
    | Stable

type Version =
    { Major: int
      Minor: int
      Patch: int
      Stage: VersionStage }

type ApiSignature = ApiSignature of string

type ApiChange =
    | Breaking of removed: ApiSignature list
    | Addition of added: ApiSignature list
    | NoChange

type ReleaseCommand =
    | Auto
    | StartAlpha
    | PromoteToBeta
    | PromoteToRC
    | PromoteToStable
    | ShowHelp

type PublishMode =
    | GitHubActions // Push tag to trigger CI/CD
    | LocalPublish // Publish to NuGet directly

type ReleaseState =
    | FirstRelease
    | HasPreviousRelease of tag: string * currentVersion: Version

type CommandResult<'a> =
    | Success of 'a
    | Failure of string

// ============================================================================
// Shell Commands
// ============================================================================

module Shell =
    let run (cmd: string) (args: string) =
        let psi = ProcessStartInfo(cmd, args)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        use p = Process.Start(psi)
        let output = p.StandardOutput.ReadToEnd()
        let error = p.StandardError.ReadToEnd()
        p.WaitForExit()

        if p.ExitCode = 0 then
            Success(output.Trim())
        else
            Failure error

    let runOrFail cmd args =
        match run cmd args with
        | Success output -> output
        | Failure error -> failwithf "Command failed: %s %s\n%s" cmd args error

    let runSilent cmd args =
        match run cmd args with
        | Success output -> Some output
        | Failure _ -> None

// ============================================================================
// Version Parsing and Formatting
// ============================================================================

module Version =
    let parse (version: string) : Version =
        let v = version.TrimStart('v')
        let parts = v.Split('-')
        let baseParts = parts.[0].Split('.')
        let major = if baseParts.Length > 0 then int baseParts.[0] else 0
        let minor = if baseParts.Length > 1 then int baseParts.[1] else 0
        let patch = if baseParts.Length > 2 then int baseParts.[2] else 0

        let stage =
            if parts.Length > 1 then
                let pre = parts.[1]
                let numMatch = Regex.Match(pre, @"(\d+)$")

                let num =
                    if numMatch.Success then
                        int numMatch.Groups.[1].Value
                    else
                        1

                if pre.StartsWith("alpha") then PreRelease(Alpha num)
                elif pre.StartsWith("beta") then PreRelease(Beta num)
                elif pre.StartsWith("rc") then PreRelease(RC num)
                else Stable
            else
                Stable

        { Major = major
          Minor = minor
          Patch = patch
          Stage = stage }

    let format (v: Version) : string =
        let base' = sprintf "%d.%d.%d" v.Major v.Minor v.Patch

        match v.Stage with
        | PreRelease(Alpha n) -> sprintf "%s-alpha.%d" base' n
        | PreRelease(Beta n) -> sprintf "%s-beta.%d" base' n
        | PreRelease(RC n) -> sprintf "%s-rc.%d" base' n
        | Stable -> base'

    let toTag (v: Version) : string = sprintf "v%s" (format v)

    let firstAlpha =
        { Major = 0
          Minor = 1
          Patch = 0
          Stage = PreRelease(Alpha 1) }

    let bumpPreRelease =
        function
        | Alpha n -> Alpha(n + 1)
        | Beta n -> Beta(n + 1)
        | RC n -> RC(n + 1)

    let nextAlphaCycle v =
        { v with
            Minor = v.Minor + 1
            Patch = 0
            Stage = PreRelease(Alpha 1) }

    let toBeta v = { v with Stage = PreRelease(Beta 1) }
    let toRC v = { v with Stage = PreRelease(RC 1) }
    let toStable v = { v with Stage = Stable }

    let bumpPatch v =
        { v with
            Patch = v.Patch + 1
            Stage = Stable }

    let bumpMinor v =
        { v with
            Minor = v.Minor + 1
            Patch = 0
            Stage = Stable }

    let bumpMajor v =
        { Major = v.Major + 1
          Minor = 0
          Patch = 0
          Stage = Stable }

    /// Returns a tuple suitable for descending sort with proper SemVer precedence:
    /// alpha < beta < rc < stable, then by prerelease number within each stage.
    let sortKey (v: Version) =
        let stageOrder, stageNum =
            match v.Stage with
            | PreRelease(Alpha n) -> 0, n
            | PreRelease(Beta n) -> 1, n
            | PreRelease(RC n) -> 2, n
            | Stable -> 3, 0

        (v.Major, v.Minor, v.Patch, stageOrder, stageNum)

// ============================================================================
// API Extraction and Comparison
// ============================================================================

module Api =
    let private extractApiScript = "scripts/extract-api.fsx"

    /// Run API extraction in a subprocess to avoid assembly loading conflicts
    let private extractFromDll (dllPath: string) : ApiSignature list =
        match Shell.run "dotnet" (sprintf "fsi %s %s" extractApiScript dllPath) with
        | Success output ->
            output.Split('\n', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map ApiSignature
            |> Array.toList
        | Failure error ->
            printfn "Warning: Failed to extract API from %s: %s" dllPath error
            []

    let extractCurrent () : ApiSignature list =
        Shell.runOrFail "dotnet" "build -c Release --verbosity quiet" |> ignore
        extractFromDll (Path.GetFullPath dllPath)

    let extractFromTag (tag: string) : ApiSignature list =
        // Use a temp directory to checkout the tag without affecting working copy
        let tempDir = Path.Combine(Path.GetTempPath(), sprintf "falco-api-check-%s" (Guid.NewGuid().ToString("N").[..7]))

        try
            // Clone the repo at the specific tag into temp dir (use file:// for proper --depth support)
            let repoPath = Directory.GetCurrentDirectory()
            Shell.runOrFail "git" (sprintf "clone --depth 1 --branch %s file://%s %s" tag repoPath tempDir) |> ignore

            // Build in the temp directory
            Shell.runOrFail "dotnet" (sprintf "build %s/src/Falco.UnionRoutes/Falco.UnionRoutes.fsproj -c Release --verbosity quiet" tempDir) |> ignore

            let tempDllPath = Path.Combine(tempDir, "src/Falco.UnionRoutes/bin/Release/net10.0/Falco.UnionRoutes.dll")
            let api = extractFromDll tempDllPath

            // Cleanup
            Directory.Delete(tempDir, true)
            api
        with ex ->
            // Cleanup on error
            if Directory.Exists(tempDir) then
                try Directory.Delete(tempDir, true) with _ -> ()
            printfn "Warning: Failed to extract API from tag %s: %s" tag ex.Message
            []

    let compare (baseline: ApiSignature list) (current: ApiSignature list) : ApiChange =
        let baselineSet = Set.ofList baseline
        let currentSet = Set.ofList current
        let removed = Set.difference baselineSet currentSet |> Set.toList
        let added = Set.difference currentSet baselineSet |> Set.toList

        match removed, added with
        | _ :: _, _ -> Breaking removed
        | [], _ :: _ -> Addition added
        | [], [] -> NoChange

// ============================================================================
// Version Bump Logic
// ============================================================================

module Bump =
    type BumpResult = { NewVersion: Version; Reason: string }

    let private isApiChanged = function Breaking _ | Addition _ -> true | NoChange -> false

    let fromApiChange (current: Version) (change: ApiChange) : BumpResult =
        match current.Stage with
        | PreRelease(RC _) when isApiChanged change ->
            { NewVersion = Version.toBeta current
              Reason = "back to beta (API changed in RC)" }
        | PreRelease pre ->
            { NewVersion =
                { current with
                    Stage = PreRelease(Version.bumpPreRelease pre) }
              Reason =
                match pre with
                | Alpha _ -> "alpha"
                | Beta _ -> "beta"
                | RC _ -> "rc" }
        | Stable ->
            match change with
            | Breaking _ ->
                { NewVersion = Version.bumpMajor current
                  Reason = "MAJOR (breaking change)" }
            | Addition _ ->
                { NewVersion = Version.bumpMinor current
                  Reason = "MINOR (new API)" }
            | NoChange ->
                { NewVersion = Version.bumpPatch current
                  Reason = "PATCH (no API changes)" }

    /// For alpha/beta: just bump the prerelease number, skip API comparison
    let private bumpPreRelease (v: Version) (pre: PreRelease) : BumpResult =
        { NewVersion = { v with Stage = PreRelease(Version.bumpPreRelease pre) }
          Reason =
            match pre with
            | Alpha n -> sprintf "alpha.%d (API changes ignored in alpha)" (n + 1)
            | Beta n -> sprintf "beta.%d (API changes ignored in beta)" (n + 1)
            | RC n -> sprintf "rc.%d" (n + 1) }

    let forCommand (state: ReleaseState) (cmd: ReleaseCommand) : BumpResult option =
        match cmd, state with
        | ShowHelp, _ -> None
        | StartAlpha, FirstRelease ->
            Some
                { NewVersion = Version.firstAlpha
                  Reason = "first alpha release" }
        | StartAlpha, HasPreviousRelease(_, v) ->
            Some
                { NewVersion = Version.nextAlphaCycle v
                  Reason = "starting new alpha cycle" }
        | PromoteToBeta, HasPreviousRelease(_, v) ->
            Some
                { NewVersion = Version.toBeta v
                  Reason = "promoting to beta" }
        | PromoteToRC, HasPreviousRelease(_, v) ->
            Some
                { NewVersion = Version.toRC v
                  Reason = "promoting to release candidate" }
        | PromoteToStable, HasPreviousRelease(_, v) ->
            Some
                { NewVersion = Version.toStable v
                  Reason = "promoting to stable" }
        | Auto, FirstRelease -> None // Need explicit alpha command for first release
        | Auto, HasPreviousRelease(_, v) ->
            match v.Stage with
            // Alpha/beta: just bump prerelease number, no API comparison needed
            | PreRelease(Alpha _ as pre)
            | PreRelease(Beta _ as pre) -> Some(bumpPreRelease v pre)
            // RC/stable: compare APIs to determine version bump
            | PreRelease(RC _)
            | Stable -> None // Signal that API comparison is needed
        | _, FirstRelease -> None // Can't promote if no previous release

// ============================================================================
// Version Control Operations (jj with git fallback for tags)
// ============================================================================

module VCS =
    let hasUncommittedChanges () =
        // jj status shows "The working copy has no changes." when clean
        match Shell.run "jj" "status" with
        | Success output -> not (output.Contains("The working copy has no changes"))
        | Failure _ -> true // Assume changes if jj fails

    let tagExists tag =
        // Tags are still git concepts, use git directly
        match Shell.run "git" "tag -l" with
        | Success output -> output.Split('\n') |> Array.contains tag
        | Failure _ -> false

    let getLatestTag () =
        match Shell.run "git" "tag -l v*" with
        | Success output when output <> "" ->
            output.Split('\n')
            |> Array.filter (fun t -> t.StartsWith("v"))
            |> Array.sortByDescending (fun t -> Version.sortKey (Version.parse t))
            |> Array.tryHead
        | _ -> None

    let getReleaseState () : ReleaseState =
        match getLatestTag () with
        | Some tag -> HasPreviousRelease(tag, Version.parse tag)
        | None -> FirstRelease

    let commitAndTag (version: Version) =
        let versionStr = Version.format version
        let tag = Version.toTag version

        // Commit the version file change
        Shell.runOrFail "jj" (sprintf "commit -m \"Release %s\"" versionStr) |> ignore

        // Move main bookmark to the new commit (now @-)
        Shell.runOrFail "jj" "bookmark set main -r @-" |> ignore

        // Create git tag on the new commit
        Shell.runOrFail "git" (sprintf "tag -a %s -m \"Release %s\"" tag versionStr)
        |> ignore

        tag

    let push tag =
        // Push using jj, then push the tag via git
        Shell.runOrFail "jj" "git push" |> ignore
        Shell.runOrFail "git" (sprintf "push origin %s" tag) |> ignore

// ============================================================================
// NuGet Operations
// ============================================================================

module NuGet =
    let artifactsDir = "artifacts"

    let pack () =
        if Directory.Exists(artifactsDir) then
            Directory.Delete(artifactsDir, true)

        Directory.CreateDirectory(artifactsDir) |> ignore

        Shell.runOrFail
            "dotnet"
            (sprintf "pack %s -c Release -o %s" fsproj artifactsDir)
        |> ignore

        Directory.GetFiles(artifactsDir, "*.nupkg")
        |> Array.tryHead
        |> Option.defaultWith (fun () -> failwith "No .nupkg file found after pack")

    let publish (nupkgPath: string) =
        // Try NUGET_API_KEY first, then try without (relies on stored credentials)
        let apiKey = Environment.GetEnvironmentVariable("NUGET_API_KEY") |> Option.ofObj

        let pushArgs =
            match apiKey with
            | Some key ->
                sprintf "nuget push %s --api-key %s --source https://api.nuget.org/v3/index.json --skip-duplicate" nupkgPath key
            | None ->
                printfn "No NUGET_API_KEY found, trying with stored credentials..."
                printfn "(To set up: get key from https://www.nuget.org/account/apikeys)"
                sprintf "nuget push %s --source https://api.nuget.org/v3/index.json --skip-duplicate" nupkgPath

        Shell.runOrFail "dotnet" pushArgs |> ignore

// ============================================================================
// File Operations
// ============================================================================

module Project =
    let updateVersion (version: Version) =
        let content = File.ReadAllText(fsproj)

        let newContent =
            Regex.Replace(content, @"<Version>.*</Version>", sprintf "<Version>%s</Version>" (Version.format version))

        File.WriteAllText(fsproj, newContent)

// ============================================================================
// User Interaction
// ============================================================================

module UI =
    let promptYesNo message =
        printf "%s [y/N] " message
        match Console.ReadLine() with
        | null -> false
        | s -> s.ToLower() = "y"

    let printApiChanges =
        function
        | Breaking removed ->
            printfn "\nBREAKING API changes:"

            removed
            |> List.truncate 10
            |> List.iter (fun (ApiSignature s) -> printfn "  - %s" s)

            if removed.Length > 10 then
                printfn "  ... and %d more" (removed.Length - 10)
        | Addition added ->
            printfn "\nNew APIs:"

            added
            |> List.truncate 10
            |> List.iter (fun (ApiSignature s) -> printfn "  + %s" s)

            if added.Length > 10 then
                printfn "  ... and %d more" (added.Length - 10)
        | NoChange -> printfn "\nNo API changes detected."

    let showHelp () =
        printfn "Usage: mise run release [command] [--publish]"
        printfn ""
        printfn "Versioning:"
        printfn "  alpha/beta  - just bump prerelease number (no API check)"
        printfn "  rc/stable   - bump based on API changes:"
        printfn "                MAJOR (breaking) / MINOR (additions) / PATCH (none)"
        printfn ""
        printfn "Commands:"
        printfn "  (none)  - auto-bump: alpha.N+1 or API-based for rc/stable"
        printfn "  alpha   - start first alpha or new alpha cycle"
        printfn "  beta    - promote to beta"
        printfn "  rc      - promote to release candidate"
        printfn "  stable  - promote to stable release"
        printfn ""
        printfn "Options:"
        printfn "  --publish  - publish to NuGet locally instead of pushing to GitHub"
        printfn ""
        printfn "For --publish, set NUGET_API_KEY environment variable:"
        printfn "  1. Get key from https://www.nuget.org/account/apikeys"
        printfn "  2. export NUGET_API_KEY=\"your-key\"  (add to ~/.zshrc)"
        printfn "  Or: NUGET_API_KEY=\"key\" mise run release --publish"

// ============================================================================
// Command Parsing
// ============================================================================

let parseCommand =
    function
    | "--help"
    | "-h" -> ShowHelp
    | "alpha" -> StartAlpha
    | "beta" -> PromoteToBeta
    | "rc" -> PromoteToRC
    | "stable" -> PromoteToStable
    | "" -> Auto
    | other -> failwithf "Unknown command: %s" other

let parseArgs (argv: string array) : ReleaseCommand * PublishMode =
    let args = argv |> Array.toList
    let hasPublish = args |> List.contains "--publish"

    let cmdArgs =
        args
        |> List.filter (fun a -> a <> "--publish")
        |> List.tryHead
        |> Option.defaultValue ""

    let cmd = parseCommand cmdArgs
    let mode = if hasPublish then LocalPublish else GitHubActions
    (cmd, mode)

// ============================================================================
// Main
// ============================================================================

type ReleaseOutcome =
    | Released of tag: string
    | Aborted
    | NeedsExplicitCommand of message: string
    | HelpShown

let release (cmd: ReleaseCommand) (mode: PublishMode) : ReleaseOutcome =
    match cmd with
    | ShowHelp ->
        UI.showHelp ()
        HelpShown
    | _ ->
        if VCS.hasUncommittedChanges () then
            failwith "You have uncommitted changes. Please commit or stash them first."

        let state = VCS.getReleaseState ()

        printfn
            "Current version: %s"
            (match state with
             | FirstRelease -> "(none)"
             | HasPreviousRelease(_, v) -> Version.format v)

        // Try to get bump without API comparison first (works for alpha/beta and explicit commands)
        let bump =
            match Bump.forCommand state cmd with
            | Some b -> Some b
            | None ->
                // API comparison needed (RC/stable Auto, or first release)
                match cmd, state with
                | Auto, HasPreviousRelease(tag, v) ->
                    printfn "\nComparing API against %s..." tag
                    let baseline = Api.extractFromTag tag
                    printfn "Building current version..."
                    let current = Api.extractCurrent ()
                    let change = Api.compare baseline current
                    UI.printApiChanges change
                    Some(Bump.fromApiChange v change)
                | Auto, FirstRelease -> None
                | _ ->
                    printfn "Building current version..."
                    Api.extractCurrent () |> ignore
                    None

        match bump with
        | None -> NeedsExplicitCommand "No previous releases. Use 'mise run release alpha' for first release."
        | Some bump ->
            let newTag = Version.toTag bump.NewVersion

            if VCS.tagExists newTag then
                failwithf "Tag %s already exists" newTag

            printfn "\nVersion bump: %s" bump.Reason
            printfn "New version: %s" (Version.format bump.NewVersion)

            match mode with
            | LocalPublish -> printfn "Mode: local publish to NuGet"
            | GitHubActions -> printfn "Mode: push to GitHub Actions"

            if not (UI.promptYesNo "Continue?") then
                Aborted
            else
                Project.updateVersion bump.NewVersion
                let tag = VCS.commitAndTag bump.NewVersion
                printfn "Created tag %s" tag

                match mode with
                | LocalPublish ->
                    printfn "\nPacking..."
                    let nupkgPath = NuGet.pack ()
                    printfn "Created %s" nupkgPath

                    if UI.promptYesNo "\nPublish to NuGet.org?" then
                        printfn "Publishing..."
                        NuGet.publish nupkgPath
                        printfn "\nPublished to NuGet.org!"

                        if UI.promptYesNo "Also push tag to GitHub?" then
                            VCS.push tag
                            printfn "Pushed tag %s" tag
                    else
                        printfn "\nTo publish later: dotnet nuget push %s --source https://api.nuget.org/v3/index.json" nupkgPath

                | GitHubActions ->
                    if UI.promptYesNo "\nPush to trigger release?" then
                        VCS.push tag
                        printfn "\nPushed! %s/actions" repoUrl
                    else
                        printfn "\nTo push: git push origin HEAD && git push origin %s" tag

                Released tag

let main (argv: string array) =
    try
        let (cmd, mode) = parseArgs argv

        match release cmd mode with
        | Released _ -> 0
        | Aborted ->
            printfn "Aborted."
            0
        | NeedsExplicitCommand msg ->
            printfn "%s" msg
            0
        | HelpShown -> 0
    with ex ->
        eprintfn "Error: %s" ex.Message
        1

main (fsi.CommandLineArgs |> Array.skip 1)
