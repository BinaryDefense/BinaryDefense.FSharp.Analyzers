module Tests

open System
open Expecto
open BinaryDefense.FSharp.Analyzers




[<Tests>]
let tests =
    testList "samples" [
        testCase "Check option analyzer" <| fun _ ->
            let analyzers = [
                Hashing.optionValueAnalyzer
            ]
            let result =
                (AnalyzerBootstrap.runProject
                    "/Users/jimmybyrd/Documents/GitHub/BinaryDefense.FSharp.Analyzers/tests/examples/hashing/hashing.fsproj"
                    []
                    analyzers).Value

            printfn "result -> %A" result

            let subject = 1 + 2
            Expect.equal subject 3 "Addition works"
    ]
