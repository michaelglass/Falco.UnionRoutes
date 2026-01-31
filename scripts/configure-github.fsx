#!/usr/bin/env dotnet fsi

open System
open System.Diagnostics

// Auto-detect repo from git remote
let getRepoFromGit () =
    let psi = ProcessStartInfo("gh", "repo view --json nameWithOwner -q .nameWithOwner")
    psi.RedirectStandardOutput <- true
    psi.UseShellExecute <- false
    use p = Process.Start(psi)
    let output = p.StandardOutput.ReadToEnd().Trim()
    p.WaitForExit()
    if p.ExitCode = 0 && output <> "" then output
    else failwith "Could not detect repository. Make sure you're in a git repo with a GitHub remote."

let repo = getRepoFromGit ()

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

let gh args = runOrFail "gh" args

let ghMayFail args =
    let (code, output, error) = run "gh" args
    (code = 0, output, error)

let printSection title =
    printfn ""
    printfn "=== %s ===" title

// Check gh is authenticated
printSection "Checking GitHub CLI authentication"
let _ = gh "auth status"
printfn "Authenticated"

// Repository settings
printSection "Configuring repository settings"

// Enable issues, discussions; disable wiki (use docs instead)
gh (sprintf "repo edit %s --enable-issues --enable-projects --delete-branch-on-merge" repo) |> ignore
printfn "- Enabled issues and projects"
printfn "- Enabled delete branch on merge"

// Set repository description and topics
gh (sprintf "repo edit %s --description \"Type-safe routing with discriminated unions for Falco\"" repo) |> ignore
printfn "- Set repository description"

let topics = [| "fsharp"; "falco"; "routing"; "discriminated-unions"; "dotnet"; "web"; "type-safe" |]
let topicsJson = sprintf """{"names":[%s]}""" (topics |> Array.map (sprintf "\"%s\"") |> String.concat ",")
let topicsFile = System.IO.Path.GetTempFileName()
System.IO.File.WriteAllText(topicsFile, topicsJson)
gh (sprintf "api repos/%s/topics -X PUT --input %s" repo topicsFile) |> ignore
System.IO.File.Delete(topicsFile)
printfn "- Set repository topics: %s" (String.concat ", " topics)

// Repository variables for Actions
printSection "Configuring Actions variables"

let setVariable name value =
    let (exists, _, _) = ghMayFail (sprintf "variable get %s --repo %s" name repo)
    if exists then
        gh (sprintf "variable set %s --repo %s --body \"%s\"" name repo value) |> ignore
        printfn "- Updated variable: %s = %s" name value
    else
        gh (sprintf "variable set %s --repo %s --body \"%s\"" name repo value) |> ignore
        printfn "- Created variable: %s = %s" name value

// Set PUBLISH_TO_NUGET to false by default (user enables when ready)
setVariable "PUBLISH_TO_NUGET" "false"

// Branch protection for main
printSection "Configuring branch protection for 'main'"

let branchProtectionJson = """
{
  "required_status_checks": {
    "strict": true,
    "contexts": ["build"]
  },
  "enforce_admins": false,
  "required_pull_request_reviews": null,
  "restrictions": null,
  "allow_force_pushes": false,
  "allow_deletions": false
}
"""

// Write JSON to temp file for the API call
let tempFile = System.IO.Path.GetTempFileName()
System.IO.File.WriteAllText(tempFile, branchProtectionJson)

let (success, _, error) = ghMayFail (sprintf "api repos/%s/branches/main/protection -X PUT --input %s" repo tempFile)
System.IO.File.Delete(tempFile)

if success then
    printfn "- Enabled branch protection:"
    printfn "  - Require status checks to pass (build)"
    printfn "  - Require branches to be up to date"
    printfn "  - Disallow force pushes"
    printfn "  - Disallow branch deletion"
else
    printfn "- Warning: Could not set branch protection (may require admin access)"
    printfn "  Error: %s" error

// Rulesets (newer alternative to branch protection)
printSection "Checking for required secrets"

printfn ""
printfn "The following secrets should be configured manually:"
printfn ""
printfn "  NUGET_API_KEY - Required for publishing to NuGet.org"
printfn "                  Get from: https://www.nuget.org/account/apikeys"
printfn ""
printfn "To add a secret:"
printfn "  gh secret set NUGET_API_KEY --repo %s" repo
printfn ""
printfn "To enable NuGet publishing after adding the secret:"
printfn "  gh variable set PUBLISH_TO_NUGET --repo %s --body \"true\"" repo

// Summary
printSection "Configuration complete"
printfn ""
printfn "Repository: https://github.com/%s" repo
printfn ""
printfn "Next steps:"
printfn "1. Add NUGET_API_KEY secret (when ready to publish)"
printfn "2. Set PUBLISH_TO_NUGET=true to enable publishing"
printfn "3. Run: mise run release patch"

0
