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
            let result =
                (AnalyzerBootstrap.runProject
                    "/Users/jimmybyrd/Documents/GitHub/BinaryDefense.FSharp.Analyzers/tests/examples/hashing/hashing.fsproj"
                    []
                    analyzers).Value
            printfn "%A" result
            Expect.hasLength result 4 "Should contain results"
    ]
