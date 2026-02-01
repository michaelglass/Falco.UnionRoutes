#!/usr/bin/env dotnet fsi

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

let parseVersion (version: string) =
    // Strip prerelease suffix for parsing base version
    let baseVersion = version.TrimStart('v').Split('-').[0]
    let parts = baseVersion.Split('.')
    if parts.Length >= 3 then
        (int parts.[0], int parts.[1], int parts.[2])
    else
        (0, 0, 0)

let bumpVersion (major, minor, patch) bumpType =
    match bumpType with
    | "major" -> (major + 1, 0, 0), None
    | "minor" -> (major, minor + 1, 0), None
    | "patch" -> (major, minor, patch + 1), None
    | "alpha" -> (major, minor, patch), Some "alpha"
    | "beta" -> (major, minor, patch), Some "beta"
    | "rc" -> (major, minor, patch), Some "rc"
    | _ -> failwith "Invalid bump type"

let isExplicitVersion (s: string) =
    // Check if it looks like a version (starts with digit or has dots)
    Regex.IsMatch(s, @"^\d+\.\d+")

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
    printfn "Usage: dotnet fsi scripts/release.fsx <version>"
    printfn ""
    printfn "Version can be:"
    printfn "  patch         - bump patch version (0.1.0 -> 0.1.1)"
    printfn "  minor         - bump minor version (0.1.0 -> 0.2.0)"
    printfn "  major         - bump major version (0.1.0 -> 1.0.0)"
    printfn "  alpha         - add alpha suffix (0.1.0 -> 0.1.0-alpha)"
    printfn "  beta          - add beta suffix (0.1.0 -> 0.1.0-beta)"
    printfn "  rc            - add rc suffix (0.1.0 -> 0.1.0-rc)"
    printfn "  <explicit>    - use explicit version (e.g., 0.1.0-alpha.1)"
    printfn ""
    printfn "Examples:"
    printfn "  mise run release 0.1.0-alpha    # first alpha release"
    printfn "  mise run release patch          # 0.1.0-alpha -> 0.1.1"
    printfn "  mise run release 1.0.0          # explicit stable release"

[<EntryPoint>]
let main argv =
    let arg =
        if argv.Length > 0 then argv.[0]
        else "patch"

    if arg = "--help" || arg = "-h" then
        showHelp ()
        exit 0

    // Get current version
    let latestTag = getLatestTag ()
    let currentVersion =
        if String.IsNullOrEmpty(latestTag) then "0.0.0"
        else latestTag.TrimStart('v')

    printfn "Current version: %s" currentVersion

    // Calculate new version
    let newVersion =
        if isExplicitVersion arg then
            // Explicit version provided
            arg
        else
            // Bump type provided
            let (major, minor, patch) = parseVersion currentVersion
            let ((newMajor, newMinor, newPatch), suffix) = bumpVersion (major, minor, patch) arg
            match suffix with
            | Some s -> sprintf "%d.%d.%d-%s" newMajor newMinor newPatch s
            | None -> sprintf "%d.%d.%d" newMajor newMinor newPatch

    let newTag = sprintf "v%s" newVersion

    printfn "New version: %s" newVersion
    printfn "New tag: %s" newTag

    // Check for uncommitted changes
    if hasUncommittedChanges () then
        eprintfn "Error: You have uncommitted changes. Please commit or stash them first."
        exit 1

    // Check if tag exists
    if tagExists newTag then
        eprintfn "Error: Tag %s already exists" newTag
        exit 1

    // Update version in .fsproj
    updateVersionInFsproj newVersion
    printfn "Updated %s to version %s" fsproj newVersion

    // Commit the version bump
    runOrFail "git" (sprintf "add %s" fsproj) |> ignore
    runOrFail "git" (sprintf "commit -m \"Bump version to %s\"" newVersion) |> ignore
    printfn "Committed version bump"

    // Create tag
    runOrFail "git" (sprintf "tag -a %s -m \"Release %s\"" newTag newVersion) |> ignore
    printfn "Created tag %s" newTag

    // Prompt to push
    printfn ""
    printfn "Ready to push. This will trigger the release workflow."

    if promptYesNo "Push to origin?" then
        runOrFail "git" "push origin HEAD" |> ignore
        runOrFail "git" (sprintf "push origin %s" newTag) |> ignore
        printfn ""
        printfn "Pushed! Release workflow will run at:"
        printfn "https://github.com/michaelglass/falco-union-routes/actions"
    else
        printfn ""
        printfn "Not pushed. To push manually:"
        printfn "  git push origin HEAD"
        printfn "  git push origin %s" newTag

    0

main (fsi.CommandLineArgs |> Array.skip 1)
