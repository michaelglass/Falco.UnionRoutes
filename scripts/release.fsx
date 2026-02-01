#!/usr/bin/env dotnet fsi

/// Release script with semantic versioning enforcement:
/// - MAJOR: removed or changed public APIs (breaking changes)
/// - MINOR: added public APIs (backward compatible)
/// - PATCH: no API changes (bug fixes, implementation changes)
///
/// Pre-release progression: alpha → beta → rc → stable
/// Default bumps the current pre-release stage, or patch if stable.

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

let fsproj = "src/Falco.UnionRoutes/Falco.UnionRoutes.fsproj"

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

let bumpPreRelease (v: Version) : Version =
    match v.PreRelease with
    | Alpha n -> { v with PreRelease = Alpha (n + 1) }
    | Beta n -> { v with PreRelease = Beta (n + 1) }
    | RC n -> { v with PreRelease = RC (n + 1) }
    | Stable -> { v with Patch = v.Patch + 1 }

let promoteToBeta (v: Version) : Version =
    { v with PreRelease = Beta 1 }

let promoteToRC (v: Version) : Version =
    { v with PreRelease = RC 1 }

let promoteToStable (v: Version) : Version =
    { v with PreRelease = Stable }

let bumpPatch (v: Version) : Version =
    { v with Patch = v.Patch + 1; PreRelease = Stable }

let bumpMinor (v: Version) : Version =
    { v with Minor = v.Minor + 1; Patch = 0; PreRelease = Stable }

let bumpMajor (v: Version) : Version =
    { Major = v.Major + 1; Minor = 0; Patch = 0; PreRelease = Stable }

let startAlpha (v: Version) : Version =
    { v with Minor = v.Minor + 1; Patch = 0; PreRelease = Alpha 1 }

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
    printfn "Semantic Versioning Rules:"
    printfn "  MAJOR  - breaking changes (removed/changed public APIs)"
    printfn "  MINOR  - new features (added public APIs, backward compatible)"
    printfn "  PATCH  - bug fixes (no API changes)"
    printfn ""
    printfn "Commands:"
    printfn "  (none)    - bump current pre-release, or patch if stable"
    printfn "  alpha     - start new alpha cycle (bumps minor)"
    printfn "  beta      - promote to beta"
    printfn "  rc        - promote to release candidate"
    printfn "  stable    - promote to stable release"
    printfn "  patch     - bump patch version (bug fix, no API changes)"
    printfn "  minor     - bump minor version (added APIs)"
    printfn "  major     - bump major version (breaking changes)"
    printfn ""
    printfn "Examples:"
    printfn "  mise run release           # 0.1.0-alpha.1 → 0.1.0-alpha.2"
    printfn "  mise run release beta      # 0.1.0-alpha.3 → 0.1.0-beta.1"
    printfn "  mise run release stable    # 0.1.0-rc.1 → 0.1.0"
    printfn "  mise run release           # 0.1.0 → 0.1.1 (patch)"
    printfn "  mise run release alpha     # 0.1.1 → 0.2.0-alpha.1"

[<EntryPoint>]
let main argv =
    let cmd = if argv.Length > 0 then argv.[0] else ""

    if cmd = "--help" || cmd = "-h" then
        showHelp ()
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

    // Calculate new version
    let newVersion =
        match cmd with
        | "" -> bumpPreRelease currentVersion
        | "alpha" -> startAlpha currentVersion
        | "beta" -> promoteToBeta currentVersion
        | "rc" -> promoteToRC currentVersion
        | "stable" -> promoteToStable currentVersion
        | "patch" -> bumpPatch currentVersion
        | "minor" -> bumpMinor currentVersion
        | "major" -> bumpMajor currentVersion
        | _ ->
            eprintfn "Unknown command: %s" cmd
            eprintfn "Run with --help for usage"
            exit 1

    let newVersionStr = formatVersion newVersion
    let newTag = sprintf "v%s" newVersionStr

    printfn "New version: %s" newVersionStr
    printfn "New tag: %s" newTag

    // Show semver reminder for major/minor
    match cmd with
    | "major" ->
        printfn ""
        printfn "MAJOR version bump - ensure this includes breaking API changes"
    | "minor" ->
        printfn ""
        printfn "MINOR version bump - ensure this adds new public APIs"
    | "patch" ->
        printfn ""
        printfn "PATCH version bump - ensure no public API changes"
    | _ -> ()

    // Check for uncommitted changes
    if hasUncommittedChanges () then
        eprintfn "Error: You have uncommitted changes. Please commit or stash them first."
        exit 1

    // Check if tag exists
    if tagExists newTag then
        eprintfn "Error: Tag %s already exists" newTag
        exit 1

    // Update version in .fsproj
    updateVersionInFsproj newVersionStr
    printfn "Updated %s to version %s" fsproj newVersionStr

    // Commit the version bump
    runOrFail "git" (sprintf "add %s" fsproj) |> ignore
    runOrFail "git" (sprintf "commit -m \"Bump version to %s\"" newVersionStr) |> ignore
    printfn "Committed version bump"

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
