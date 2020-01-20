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
            let hashed = examples.Hashing.hello "hello"

            let file = IO.Path.Combine(__SOURCE_DIRECTORY__ ,"../examples/hashing/Library.fs")

            let result =
                (AnalyzerBootstrap.runProject
                    file
                    analyzers).Value

            Expect.hasLength result 7 "Should contain results"
    ]
