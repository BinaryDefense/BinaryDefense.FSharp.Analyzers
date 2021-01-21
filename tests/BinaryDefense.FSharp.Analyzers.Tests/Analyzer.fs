module AnalyzerBootstrap

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open Ionide.ProjInfo
open Ionide.ProjInfo.Types


let checker =
    FSharpChecker.Create(
        projectCacheSize = 200,
        keepAllBackgroundResolutions = true,
        keepAssemblyContents = true,
        ImplicitlyStartBackgroundWork = true)

// let projectSystem = ProjectController(checker)

let dumpOpts (opts :  FSharpProjectOptions) =
    printfn  "FSharpProjectOptions.OtherOptions ->"
    opts.OtherOptions
    |> Array.iter(printfn "%s")

/// This will fix relative paths and make them absolute
let toAbsolutePath path =
    FileInfo(path).FullName


let loadProject toolsPath projPath =
    async {
        let projPath = toAbsolutePath projPath
        let loader = WorkspaceLoader.Create(toolsPath)
        // Uncomment for debugging
        loader.Notifications |> Observable.add(fun e -> printfn "%A" e)
        let parsed = loader.LoadProject projPath |> Seq.toList
        let opts = FCS.mapToFSharpProjectOptions parsed.Head parsed
        return
            opts.SourceFiles
            |> Array.map(fun file ->
                file, opts
            )
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

let tee f x =
    f x
    x
open Microsoft.FSharp.Reflection
open System.Reflection
let nullToolPath () =
    let case =
        FSharpType.GetUnionCases(typeof<ToolsPath>, BindingFlags.NonPublic)
        |> Seq.head


    FSharpValue.MakeUnion(case,[| box ""|], BindingFlags.NonPublic) :?> ToolsPath

type Loader =
| Project of string
| File of string
let toolsPath = lazy (
    try
        Init.init ()
    with e -> nullToolPath ()
)
let runProject (proj : Loader) (analyzers : Analyzer seq)  =
    // let path =
    //     Path.Combine(Environment.CurrentDirectory, proj)
    //     |> Path.GetFullPath
    let files =
        match proj with
        | Project f -> loadProject toolsPath.Value f
        | File f -> [| loadFile f |]
        |> tee (fun fs -> fs |> Seq.iter(fun (file, opts) ->  printfn "%s" file))
        |> Array.choose typeCheckFile
        |> Array.choose createContext

    files
    |> Array.collect(fun ctx ->
        analyzers
        |> Seq.collect (fun analyzer -> analyzer ctx)
        |> Seq.toArray
    )
