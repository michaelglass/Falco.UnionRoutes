#!/usr/bin/env dotnet fsi

/// Check per-file test coverage from Cobertura XML report.
/// Fails if any source file is below the minimum coverage threshold.

open System
open System.IO
open System.Xml.Linq

// ============================================================================
// Configuration
// ============================================================================

let minCoverage = 100.0 // Minimum per-file coverage percentage
let coverageDir = "coverage"
// Only check main library .fs files (exclude test files)
let includedExtensions = [| ".fs" |]
let excludedPatterns = [| "Test"; "AssemblyInfo"; "AssemblyAttributes" |]

// ============================================================================
// Types
// ============================================================================

type FileCoverage =
    { FileName: string
      LineRate: float
      Percentage: float }

type CoverageResult =
    | AllPassed of files: FileCoverage list
    | SomeFailed of passed: FileCoverage list * failed: FileCoverage list

// ============================================================================
// Coverage Parsing
// ============================================================================

let findCoverageFile () : string option =
    if Directory.Exists(coverageDir) then
        Directory.GetFiles(coverageDir, "coverage.cobertura.xml", SearchOption.AllDirectories)
        |> Array.sortByDescending File.GetLastWriteTime
        |> Array.tryHead
    else
        None

let parseFileCoverage (packageElement: XElement) : FileCoverage list =
    let ns = packageElement.Name.Namespace

    packageElement.Descendants(ns + "class")
    |> Seq.choose (fun classEl ->
        let fileName = classEl.Attribute(XName.Get("filename"))
        let lineRate = classEl.Attribute(XName.Get("line-rate"))

        match fileName, lineRate with
        | null, _
        | _, null -> None
        | fn, lr ->
            let rate = Double.Parse(lr.Value)

            Some
                { FileName = fn.Value
                  LineRate = rate
                  Percentage = rate * 100.0 })
    |> Seq.toList

let parseCoverageReport (xmlPath: string) : FileCoverage list =
    let doc = XDocument.Load(xmlPath)
    let ns = doc.Root.Name.Namespace

    let isIncluded (f: FileCoverage) =
        let hasValidExt = includedExtensions |> Array.exists f.FileName.EndsWith
        let isExcluded = excludedPatterns |> Array.exists f.FileName.Contains
        hasValidExt && not isExcluded

    doc.Root.Descendants(ns + "package")
    |> Seq.collect parseFileCoverage
    |> Seq.distinctBy (fun f -> f.FileName)
    |> Seq.filter isIncluded
    |> Seq.toList

// ============================================================================
// Coverage Check
// ============================================================================

let checkCoverage (files: FileCoverage list) : CoverageResult =
    let passed, failed = files |> List.partition (fun f -> f.Percentage >= minCoverage)

    if List.isEmpty failed then
        AllPassed passed
    else
        SomeFailed(passed, failed)

// ============================================================================
// Output
// ============================================================================

let printResults (result: CoverageResult) =
    let printFile prefix (f: FileCoverage) =
        let shortName = Path.GetFileName(f.FileName)
        printfn "%s %s: %.1f%%" prefix shortName f.Percentage

    match result with
    | AllPassed files ->
        printfn "✓ All %d files meet minimum coverage of %.0f%%\n" files.Length minCoverage
        files |> List.sortBy (fun f -> f.FileName) |> List.iter (printFile "  ✓")

    | SomeFailed(passed, failed) ->
        printfn "✗ %d file(s) below minimum coverage of %.0f%%\n" failed.Length minCoverage

        printfn "Failed:"
        failed |> List.sortBy (fun f -> f.Percentage) |> List.iter (printFile "  ✗")

        if not (List.isEmpty passed) then
            printfn "\nPassed:"
            passed |> List.sortBy (fun f -> f.FileName) |> List.iter (printFile "  ✓")

// ============================================================================
// Main
// ============================================================================

let exitCode =
    match findCoverageFile () with
    | None ->
        eprintfn "No coverage report found in %s/" coverageDir
        eprintfn "Run 'mise run test' first to generate coverage data."
        1

    | Some xmlPath ->
        printfn "Checking coverage from: %s\n" xmlPath

        let files = parseCoverageReport xmlPath
        let result = checkCoverage files

        printResults result

        match result with
        | AllPassed _ -> 0
        | SomeFailed _ -> 1

exit exitCode
