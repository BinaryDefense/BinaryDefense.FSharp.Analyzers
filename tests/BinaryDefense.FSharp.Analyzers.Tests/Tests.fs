module Tests

open System
open Expecto
open BinaryDefense.FSharp.Analyzers

let analyzers = [ Hashing.weakHashingAnalyzer ]

let testSeq name tests = testList name (List.ofSeq tests)

[<Tests>]
let tests =
    testSeq "Hashing Tests" <|
        testParam analyzers [
            "Check MD5.Create() binding", fun analyzers () ->
                let file = IO.Path.Combine(__SOURCE_DIRECTORY__ ,"../examples/hashing/md5create.fs")
                let results = (AnalyzerBootstrap.runProject (AnalyzerBootstrap.File file) analyzers).Value
                Expect.hasLength results 1 ""
                results
                |> Seq.iter(fun r ->
                    Expect.stringContains r.Message "MD5" ""
                )
            "Check MD5CryptoServiceProvider binding", fun analyzers () ->
                let file = IO.Path.Combine(__SOURCE_DIRECTORY__ ,"../examples/hashing/md5CryptoServicebinding.fs")
                let results = (AnalyzerBootstrap.runProject (AnalyzerBootstrap.File file) analyzers).Value
                Expect.hasLength results 4 ""
                results
                |> Seq.iter(fun r ->
                    Expect.stringContains r.Message "MD5" ""
                )
            "Check SHA1.Create() binding", fun analyzers () ->
                let file = IO.Path.Combine(__SOURCE_DIRECTORY__ ,"../examples/hashing/sha1create.fs")
                let results = (AnalyzerBootstrap.runProject (AnalyzerBootstrap.File file) analyzers).Value
                Expect.hasLength results 1 ""
                results
                |> Seq.iter(fun r ->
                    Expect.stringContains r.Message "SHA1" ""
                )
            "Check SHACryptoServiceProvider binding", fun analyzers () ->
                let file = IO.Path.Combine(__SOURCE_DIRECTORY__ ,"../examples/hashing/sha1CryptoServicebinding.fs")
                let results = (AnalyzerBootstrap.runProject (AnalyzerBootstrap.File file) analyzers).Value
                Expect.hasLength results 4 ""
                results
                |> Seq.iter(fun r ->
                    Expect.stringContains r.Message "SHA1" ""
                )
            "Check SHACryptoServiceProvider ctor", fun analyzers () ->
                let file = IO.Path.Combine(__SOURCE_DIRECTORY__ ,"../examples/hashing/sha1CryptoServicector.fs")
                let results = (AnalyzerBootstrap.runProject (AnalyzerBootstrap.File file) analyzers).Value
                Expect.hasLength results 1 ""
                results
                |> Seq.iter(fun r ->
                    Expect.stringContains r.Message "SHA1" ""
                )
            "Check project", fun analyzers () ->
                let file = IO.Path.Combine(__SOURCE_DIRECTORY__, "../examples/hashing/hashing.fsproj")
                // let file = "/Users/jimmybyrd/Documents/GitHub/MiniScaffold/Content/Library/src/MyLib.1/MyLib.1.fsproj"
                let results = (AnalyzerBootstrap.runProject (AnalyzerBootstrap.Project file) analyzers).Value
                Expect.isGreaterThan results.Length 0 ""
            ]
