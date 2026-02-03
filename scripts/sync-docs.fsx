#!/usr/bin/env dotnet fsi

/// Syncs content from README.md to docs/index.md.
/// Tagged sections in README.md are extracted and replace corresponding sections in index.md.
///
/// Usage:
///   dotnet fsi scripts/sync-docs.fsx          # Update index.md
///   dotnet fsi scripts/sync-docs.fsx --check  # Check if in sync (for CI)
///
/// Tag format:
///   README.md:    <!-- sync:section-name:start --> ... <!-- sync:section-name:end -->
///   index.md:     <!-- sync:section-name --> ... <!-- sync:section-name:end -->

open System
open System.IO
open System.Text.RegularExpressions

// ============================================================================
// Configuration
// ============================================================================

let readmePath = "README.md"
let indexPath = "docs/index.md"

// ============================================================================
// Types
// ============================================================================

type Section = { Name: string; Content: string }

type SyncResult =
    | InSync
    | OutOfSync of expected: string * actual: string
    | Updated

// ============================================================================
// Parsing
// ============================================================================

let extractSections (content: string) : Map<string, string> =
    let pattern = @"<!-- sync:(\w+):start -->\s*\n([\s\S]*?)<!-- sync:\1:end -->"
    let matches = Regex.Matches(content, pattern)

    matches
    |> Seq.cast<Match>
    |> Seq.map (fun m ->
        let name = m.Groups[1].Value
        let sectionContent = m.Groups[2].Value.TrimEnd()
        name, sectionContent)
    |> Map.ofSeq

let replaceSections (content: string) (sections: Map<string, string>) : string =
    let mutable result = content

    for KeyValue(name, newContent) in sections do
        let pattern = $@"(<!-- sync:{name} -->\s*\n)[\s\S]*?(<!-- sync:{name}:end -->)"
        let replacement = $"$1{newContent}\n$2"
        result <- Regex.Replace(result, pattern, replacement)

    result

// ============================================================================
// Main Logic
// ============================================================================

let sync (check: bool) : int =
    if not (File.Exists readmePath) then
        eprintfn "Error: %s not found" readmePath
        1
    elif not (File.Exists indexPath) then
        eprintfn "Error: %s not found" indexPath
        1
    else
        let readmeContent = File.ReadAllText readmePath
        let indexContent = File.ReadAllText indexPath

        let sections = extractSections readmeContent

        if sections.IsEmpty then
            eprintfn "Warning: No sync sections found in %s" readmePath
            eprintfn "Add sections with: <!-- sync:section-name:start --> ... <!-- sync:section-name:end -->"
            0
        else
            printfn "Found %d sync section(s): %s" sections.Count (sections.Keys |> String.concat ", ")

            let updatedIndex = replaceSections indexContent sections

            if check then
                if updatedIndex = indexContent then
                    printfn "OK: %s is in sync with %s" indexPath readmePath
                    0
                else
                    eprintfn "Error: %s is out of sync with %s" indexPath readmePath
                    eprintfn "Run 'dotnet fsi scripts/sync-docs.fsx' to update"
                    1
            else
                if updatedIndex = indexContent then
                    printfn "Already in sync, no changes needed"
                    0
                else
                    File.WriteAllText(indexPath, updatedIndex)
                    printfn "Updated %s" indexPath
                    0

// ============================================================================
// Entry Point
// ============================================================================

let args = fsi.CommandLineArgs |> Array.skip 1 // Skip script name
let check = args |> Array.contains "--check"

exit (sync check)
