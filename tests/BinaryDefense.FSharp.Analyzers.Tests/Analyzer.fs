module AnalyzerBootstrap

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open ProjectSystem
open GlobExpressions

let checker =
    FSharpChecker.Create(
        projectCacheSize = 200,
        keepAllBackgroundResolutions = true,
        keepAssemblyContents = true,
        ImplicitlyStartBackgroundWork = true)

let projectSystem = ProjectController(checker)

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
                // printError "Project loading failed: %A" errorDetails
                None
            | ProjectResponse.ProjectLoading(_)
            | ProjectResponse.WorkspaceLoad(_) ->
                None

        return filesToCheck
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


let runProject proj (globs: Glob list) analyzers  =
    let path =
        Path.Combine(Environment.CurrentDirectory, proj)
        |> Path.GetFullPath

    match loadProject path with
    | None -> None
    | Some files ->

        let files =
            files
            |> List.filter (fun (f,_) ->
                match globs |> List.tryFind (fun g -> g.IsMatch f) with
                | None -> true
                | Some g ->
                    // printInfo "Ignoring file %s for pattern %s" f g.Pattern
                    false)
            |> List.choose typeCheckFile
            |> List.choose createContext


        files
        |> Seq.collect (fun ctx ->
            // printInfo "Running analyzers for %s" ctx.FileName
            analyzers |> Seq.collect (fun analyzer -> analyzer ctx)
        )
        |> Seq.toList
        |> Some
