module Tests

open System
open Expecto
open BinaryDefense.FSharp.Analyzers




[<Tests>]
let tests =
    testList "Hashing Tests" [
        testCase "Check hashing analyzer" <| fun _ ->
            let analyzers = [
                Hashing.weakHashingAnalyzer
            ]
            let file = IO.Path.Combine(__SOURCE_DIRECTORY__ ,"../examples/hashing/Library.fs")

            let result =
                (AnalyzerBootstrap.runProject
                    file
                    analyzers).Value
            // printfn "%A" result
            Expect.hasLength result 4 "Should contain results"
    ]
