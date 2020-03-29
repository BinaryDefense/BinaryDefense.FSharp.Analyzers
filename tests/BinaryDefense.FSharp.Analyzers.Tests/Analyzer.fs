module AnalyzerBootstrap

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open ProjectSystem

let checker =
    FSharpChecker.Create(
        projectCacheSize = 200,
        keepAllBackgroundResolutions = true,
        keepAssemblyContents = true,
        ImplicitlyStartBackgroundWork = true)

let projectSystem = ProjectController(checker)

let dumpOpts (opts :  FSharpProjectOptions) =
    printfn  "FSharpProjectOptions.OtherOptions ->"
    opts.OtherOptions
    |> Array.iter(printfn "%s")

/// This will fix relative paths and make them absolute
let toAbsolutePath path =
    FileInfo(path).FullName

let loadProject file =
    async {
        let! projLoading = projectSystem.LoadProject file ignore FSIRefs.TFM.NetCore (fun _ _ _ -> ())
        let filesToCheck =
            match projLoading with
            | ProjectResponse.Project proj ->
                // printInfo "Project %s loaded" file
                proj.projectFiles
                |> List.choose (fun file ->
                    projectSystem.GetProjectOptions file
                    |> Option.map (fun opts -> file, opts)
                )
                |> Some
            | ProjectResponse.ProjectError(errorDetails) ->
                printfn "Project loading failed: %A" errorDetails
                None
            | ProjectResponse.ProjectLoading(_)
            | ProjectResponse.WorkspaceLoad(_) ->
                None

        return filesToCheck
    } |> Async.RunSynchronously

let loadFile file =
    async {
        let! source = IO.File.ReadAllTextAsync file |> Async.AwaitTask
        let! (opts, error) = checker.GetProjectOptionsFromScript(file, SourceText.ofString source, assumeDotNetFramework = false, useSdkRefs = true, useFsiAuxLib = true, otherFlags = [|"--targetprofile:netstandard" |])
        //HACK: fixes references on windows. See https://github.com/fsharp/FSharp.Compiler.Service/issues/920#issuecomment-576355140
        let newOO =
            opts.OtherOptions
            |> Array.map(fun i ->
                if i.StartsWith("-r:") then
                    i.Split("-r:", StringSplitOptions.RemoveEmptyEntries)
                    |> Array.head
                    |> toAbsolutePath
                    |> sprintf "-r:%s"
                else
                    i
            )
        // dumpOpts opts
        return file,{ opts with OtherOptions = newOO}
    } |> Async.RunSynchronously

let typeCheckFile (file,opts) =
    let text = File.ReadAllText file
    let st = SourceText.ofString text
    let (parseRes, checkAnswer) =
        checker.ParseAndCheckFileInProject(file, 1, st, opts)
        |> Async.RunSynchronously

    match checkAnswer with
    | FSharpCheckFileAnswer.Aborted ->
        // printError "Checking of file %s aborted" file
        None
    | FSharpCheckFileAnswer.Succeeded(c) ->
        Some (file, text, parseRes, c)

let entityCache = EntityCache()

let getAllEntities (checkResults: FSharpCheckFileResults) (publicOnly: bool) : AssemblySymbol list =
    try
        let res = [
          yield! AssemblyContentProvider.getAssemblySignatureContent AssemblyContentType.Full checkResults.PartialAssemblySignature
          let ctx = checkResults.ProjectContext
          let assembliesByFileName =
            ctx.GetReferencedAssemblies()
            |> Seq.groupBy (fun asm -> asm.FileName)
            |> Seq.map (fun (fileName, asms) -> fileName, List.ofSeq asms)
            |> Seq.toList
            |> List.rev // if mscorlib.dll is the first then FSC raises exception when we try to
                        // get Content.Entities from it.

          for fileName, signatures in assembliesByFileName do
            let contentType = if publicOnly then Public else Full
            let content = AssemblyContentProvider.getAssemblyContent entityCache.Locking contentType fileName signatures
            yield! content
        ]
        res
    with
    | _ -> []

let createContext (file, text: string, p: FSharpParseFileResults,c: FSharpCheckFileResults) =
    match p.ParseTree, c.ImplementationFile with
    | Some pt, Some tast ->
        let context : Context = {
            FileName = file
            Content = text.Split([|'\n'|])
            ParseTree = pt
            TypedTree = tast
            Symbols = c.PartialAssemblySignature.Entities |> Seq.toList
            GetAllEntities = getAllEntities c
        }
        Some context
    | _ -> None

type Loader =
| Project of string
| File of string

let runProject (proj : Loader) (analyzers : Analyzer seq)  =
    // let path =
    //     Path.Combine(Environment.CurrentDirectory, proj)
    //     |> Path.GetFullPath

    let files =
        match proj with
        | Project f -> loadProject f
        | File f -> Some [ loadFile f ]
        |> Option.map(List.choose typeCheckFile)
        |> Option.map(List.choose createContext)

    files
    |> Option.map(List.collect(fun ctx ->
        analyzers
        |> Seq.collect (fun analyzer -> analyzer ctx)
        |> Seq.toList
    ))
