#!/usr/bin/env dotnet fsi

/// Extracts public API signatures from a .NET assembly.
/// Used by release.fsx to compare APIs across versions.
///
/// Usage: dotnet fsi scripts/extract-api.fsx <path-to-dll>
/// Output: One API signature per line, sorted alphabetically

#r "nuget: System.Reflection.MetadataLoadContext"

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices

/// Gets common assembly search paths including SDK, runtime, shared framework, and NuGet packages
let getAssemblySearchPaths (dllPath: string) =
    let dllDir = Path.GetDirectoryName(dllPath)
    let runtimeDir = RuntimeEnvironment.GetRuntimeDirectory()

    // Walk up from runtime dir to find "shared" parent
    let dotnetRoot =
        let mutable dir = DirectoryInfo(runtimeDir)

        while dir <> null && dir.Name <> "shared" do
            dir <- dir.Parent

        if dir <> null && dir.Parent <> null then
            dir.Parent.FullName
        else
            Path.GetDirectoryName(runtimeDir)

    // SDK path (for FSharp.Core)
    let sdkPaths =
        let sdkDir = Path.Combine(dotnetRoot, "sdk")

        if Directory.Exists(sdkDir) then
            Directory.GetDirectories(sdkDir)
            |> Array.sortDescending
            |> Array.truncate 1
            |> Array.collect (fun sdkVersion ->
                [| sdkVersion
                   Path.Combine(sdkVersion, "FSharp") |])
            |> Array.toList
        else
            []

    // Shared framework paths
    let sharedPaths =
        let sharedDir = Path.Combine(dotnetRoot, "shared")

        if Directory.Exists(sharedDir) then
            Directory.GetDirectories(sharedDir)
            |> Array.collect (fun frameworkDir ->
                Directory.GetDirectories(frameworkDir)
                |> Array.sortDescending
                |> Array.truncate 1)
            |> Array.toList
        else
            []

    // NuGet packages (for third-party dependencies like Falco)
    let nugetPaths =
        let nugetDir =
            Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".nuget", "packages")

        if Directory.Exists(nugetDir) then
            // Get all package versions, prefer lib/net10.0, lib/net9.0, lib/net8.0, lib/netstandard2.1, lib/netstandard2.0
            let targetFrameworks =
                [| "net10.0"
                   "net9.0"
                   "net8.0"
                   "net7.0"
                   "net6.0"
                   "netstandard2.1"
                   "netstandard2.0" |]

            Directory.GetDirectories(nugetDir)
            |> Array.collect (fun packageDir ->
                Directory.GetDirectories(packageDir)
                |> Array.sortDescending
                |> Array.truncate 1
                |> Array.collect (fun versionDir ->
                    let libDir = Path.Combine(versionDir, "lib")

                    if Directory.Exists(libDir) then
                        targetFrameworks
                        |> Array.map (fun tf -> Path.Combine(libDir, tf))
                        |> Array.filter Directory.Exists
                        |> Array.truncate 1
                    else
                        [||]))
            |> Array.toList
        else
            []

    [ dllDir; runtimeDir ] @ sdkPaths @ sharedPaths @ nugetPaths

/// Creates a resolver that searches multiple paths
let createResolver (dllPath: string) =
    let searchPaths = getAssemblySearchPaths dllPath

    { new MetadataAssemblyResolver() with
        member _.Resolve(context, assemblyName) =
            searchPaths
            |> List.tryPick (fun dir ->
                let path = Path.Combine(dir, assemblyName.Name + ".dll")
                if File.Exists(path) then Some path else None)
            |> Option.map context.LoadFromAssemblyPath
            |> Option.defaultValue null }

/// Formats a type name including generic type arguments, e.g. "FSharpFunc<String, FSharpResult<Object, String>>"
let rec formatTypeName (t: Type) =
    if t.IsGenericType then
        let baseName = t.Name.Substring(0, t.Name.IndexOf('`'))

        let args =
            t.GetGenericArguments()
            |> Array.map formatTypeName
            |> String.concat ", "

        sprintf "%s<%s>" baseName args
    elif t.IsArray then
        sprintf "%s[]" (formatTypeName (t.GetElementType()))
    else
        t.Name

let extractFromAssembly (dllPath: string) : string list =
    use context = new MetadataLoadContext(createResolver dllPath)
    let assembly = context.LoadFromAssemblyPath(dllPath)

    assembly.GetExportedTypes()
    |> Array.collect (fun t ->
        let typeSig = sprintf "type %s" t.FullName

        let memberSigs =
            t.GetMembers(
                BindingFlags.Public
                ||| BindingFlags.Instance
                ||| BindingFlags.Static
                ||| BindingFlags.DeclaredOnly
            )
            |> Array.choose (fun m ->
                match m.MemberType with
                | MemberTypes.Method ->
                    let mi = m :?> MethodInfo

                    if mi.IsSpecialName then
                        None
                    else
                        let params' =
                            mi.GetParameters()
                            |> Array.map (fun p -> formatTypeName p.ParameterType)
                            |> String.concat ", "

                        Some(sprintf "  %s::%s(%s): %s" t.FullName mi.Name params' (formatTypeName mi.ReturnType))
                | MemberTypes.Property ->
                    let pi = m :?> PropertyInfo
                    Some(sprintf "  %s::%s: %s" t.FullName pi.Name (formatTypeName pi.PropertyType))
                | MemberTypes.Field ->
                    let fi = m :?> FieldInfo

                    if fi.IsPublic then
                        Some(sprintf "  %s::%s: %s" t.FullName fi.Name (formatTypeName fi.FieldType))
                    else
                        None
                | MemberTypes.Constructor ->
                    let ci = m :?> ConstructorInfo

                    let params' =
                        ci.GetParameters()
                        |> Array.map (fun p -> formatTypeName p.ParameterType)
                        |> String.concat ", "

                    Some(sprintf "  %s::.ctor(%s)" t.FullName params')
                | _ -> None)

        Array.append [| typeSig |] memberSigs)
    |> Array.toList
    |> List.sort

let main (argv: string array) =
    if argv.Length < 1 then
        eprintfn "Usage: dotnet fsi scripts/extract-api.fsx <path-to-dll>"
        1
    else
        let dllPath = Path.GetFullPath(argv.[0])

        if not (File.Exists dllPath) then
            eprintfn "Error: File not found: %s" dllPath
            1
        else
            try
                let signatures = extractFromAssembly dllPath
                signatures |> List.iter (printfn "%s")
                0
            with ex ->
                eprintfn "Error extracting API: %s" ex.Message
                1

exit (main (fsi.CommandLineArgs |> Array.skip 1))
