namespace BinaryDefense.FSharp.Analyzers
open System.Security.Cryptography
open System.Text
module Hashing =
    open System
    open FSharp.Analyzers.SDK
    open FSharp.Compiler.SourceCodeServices
    open FSharp.Compiler.Range


    let rec visitExpr memberCallHandler (e:FSharpExpr) =
        // printfn "e -> %A" e
        match e with
        | BasicPatterns.AddressOf(lvalueExpr) ->
            visitExpr memberCallHandler lvalueExpr
        | BasicPatterns.AddressSet(lvalueExpr, rvalueExpr) ->
            visitExpr memberCallHandler lvalueExpr; visitExpr memberCallHandler rvalueExpr
        | BasicPatterns.Application(funcExpr, typeArgs, argExprs) ->
            visitExpr memberCallHandler funcExpr; visitExprs memberCallHandler argExprs
        | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
            // printfn "objExprOpt -> %A " objExprOpt
            // printfn "memberOrFunc -> %A" memberOrFunc
            // printfn "typeArgs1 -> %A " typeArgs1
            // printfn "typeArgs2 -> %A " typeArgs2
            // printfn "argExprs -> %A " argExprs
            memberCallHandler e.Range memberOrFunc
            visitObjArg memberCallHandler objExprOpt; visitExprs memberCallHandler argExprs
        | BasicPatterns.Coerce(targetType, inpExpr) ->
            visitExpr memberCallHandler inpExpr
        | BasicPatterns .FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) ->
            visitExpr memberCallHandler startExpr; visitExpr memberCallHandler limitExpr; visitExpr memberCallHandler consumeExpr
        | BasicPatterns.ILAsm(asmCode, typeArgs, argExprs) ->
            visitExprs memberCallHandler argExprs
        | BasicPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) ->
            visitObjArg memberCallHandler objExprOpt
        | BasicPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) ->
            visitObjArg memberCallHandler objExprOpt
        | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
            visitExpr memberCallHandler guardExpr; visitExpr memberCallHandler thenExpr; visitExpr memberCallHandler elseExpr
        | BasicPatterns.Lambda(lambdaVar, bodyExpr) ->
            visitExpr memberCallHandler bodyExpr
        | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) ->
            visitExpr memberCallHandler bindingExpr; visitExpr memberCallHandler bodyExpr
        | BasicPatterns.LetRec(recursiveBindings, bodyExpr) ->
            List.iter (snd >> visitExpr memberCallHandler) recursiveBindings; visitExpr memberCallHandler bodyExpr
        | BasicPatterns.NewArray(arrayType, argExprs) ->
            visitExprs memberCallHandler argExprs
        | BasicPatterns.NewDelegate(delegateType, delegateBodyExpr) ->
            visitExpr memberCallHandler delegateBodyExpr
        | BasicPatterns.NewObject(objType, typeArgs, argExprs) ->
            // printfn "NewObject.objType -> %A" objType
            memberCallHandler e.Range objType
            visitExprs memberCallHandler argExprs
        | BasicPatterns.NewRecord(recordType, argExprs) ->
            visitExprs memberCallHandler argExprs
        | BasicPatterns.NewTuple(tupleType, argExprs) ->
            visitExprs memberCallHandler argExprs
        | BasicPatterns.NewUnionCase(unionType, unionCase, argExprs) ->
            visitExprs memberCallHandler argExprs
        | BasicPatterns.Quote(quotedExpr) ->
            visitExpr memberCallHandler quotedExpr
        | BasicPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) ->
            visitObjArg memberCallHandler objExprOpt
        | BasicPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) ->
            visitObjArg memberCallHandler objExprOpt; visitExpr memberCallHandler argExpr
        | BasicPatterns.Sequential(firstExpr, secondExpr) ->
            visitExpr memberCallHandler firstExpr; visitExpr memberCallHandler secondExpr
        | BasicPatterns.TryFinally(bodyExpr, finalizeExpr) ->
            visitExpr memberCallHandler bodyExpr; visitExpr memberCallHandler finalizeExpr
        | BasicPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr) ->
            visitExpr memberCallHandler bodyExpr; visitExpr memberCallHandler catchExpr
        | BasicPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) ->
            visitExpr memberCallHandler tupleExpr
        | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) ->
            visitExpr memberCallHandler decisionExpr; List.iter (snd >> visitExpr memberCallHandler) decisionTargets
        | BasicPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) ->
            visitExprs memberCallHandler decisionTargetExprs
        | BasicPatterns.TypeLambda(genericParam, bodyExpr) ->
            visitExpr memberCallHandler bodyExpr
        | BasicPatterns.TypeTest(ty, inpExpr) ->
            visitExpr memberCallHandler inpExpr
        | BasicPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) ->
            visitExpr memberCallHandler unionExpr; visitExpr memberCallHandler valueExpr
        | BasicPatterns.UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) ->
            visitExpr memberCallHandler unionExpr
        | BasicPatterns.UnionCaseTest(unionExpr, unionType, unionCase) ->
            visitExpr memberCallHandler unionExpr
        | BasicPatterns.UnionCaseTag(unionExpr, unionType) ->
            visitExpr memberCallHandler unionExpr
        | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) ->
            visitExpr memberCallHandler baseCallExpr
            List.iter (visitObjMember memberCallHandler) overrides
            List.iter (snd >> List.iter (visitObjMember memberCallHandler)) interfaceImplementations
        | BasicPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argTypes, argExprs) ->
            visitExprs memberCallHandler argExprs
        | BasicPatterns.ValueSet(valToSet, valueExpr) ->
            visitExpr memberCallHandler valueExpr
        | BasicPatterns.WhileLoop(guardExpr, bodyExpr) ->
            visitExpr memberCallHandler guardExpr; visitExpr memberCallHandler bodyExpr
        | BasicPatterns.BaseValue baseType ->
            // printfn "BaseValue: %A" baseType;
            ()
        | BasicPatterns.DefaultValue defaultType ->
            // printfn "DefaultValue: %A" defaultType;
            ()
        | BasicPatterns.ThisValue thisType ->
            // printfn "ThisValue: %A" thisType;
            ()
        | BasicPatterns.Const(constValueObj, constType) ->
            // printfn "Const: %A" (constValueObj, constType);
            ()
        | BasicPatterns.Value(valueToGet) ->
            // printfn "Value: %A" valueToGet;
            memberCallHandler e.Range valueToGet
        | other ->
            // printfn "Other: %A" other;
            ()

    and visitExprs f exprs =
        List.iter (visitExpr f) exprs

    and visitObjArg f objOpt =
        Option.iter (visitExpr f) objOpt

    and visitObjMember f memb =
        visitExpr f memb.Body

    let rec visitDeclaration f d =
        match d with
        | FSharpImplementationFileDeclaration.Entity (e, subDecls) ->
            for subDecl in subDecls do
                visitDeclaration f subDecl
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) ->
            visitExpr f e
        | FSharpImplementationFileDeclaration.InitAction(e) ->
            visitExpr f e

    type WeakHash =
    | MD5
    | SHA1

    let toCode = function
    | MD5 -> "BDH0001"
    | SHA1 -> "BDH0002"

    // let waitForDebuggerAttached (programName) =
    // #if DEBUG
    //     if not(System.Diagnostics.Debugger.IsAttached) then
    //       printfn "Please attach a debugger for %s, PID: %d" programName (System.Diagnostics.Process.GetCurrentProcess().Id)
    //     while not(System.Diagnostics.Debugger.IsAttached) do
    //       System.Threading.Thread.Sleep(100)
    //     System.Diagnostics.Debugger.Break()
    // #else
    // ()
    // #endif

    let forTheReflectionGods () =
        use sha1 = SHA1.Create()
        sha1.ComputeHash(UTF8Encoding().GetBytes("foo"))

    let matchers =
        [
            "System.Security.Cryptography.MD5CryptoServiceProvider", MD5
            "System.Security.Cryptography.MD5.Create", MD5
            "System.Security.Cryptography.SHA1CryptoServiceProvider", SHA1
            (sprintf "%s.Create" typedefof<SHA1>.FullName), SHA1
        ] |> dict

    forTheReflectionGods () |> printfn "%A"
    [<Analyzer>]
    let weakHashingAnalyzer : Analyzer =
        // waitForDebuggerAttached ("App")
        fun ctx ->
            if ctx.FileName.EndsWith("AssemblyInfo.fs") then
                []
            else
                let state = ResizeArray<WeakHash * range>()
                let handler (range: range) (m: FSharpMemberOrFunctionOrValue) =
                    printfn "DeclaringEntity --> %A" m.DeclaringEntity
                    printfn "TryGetFullDisplayName -> %A" <| m.TryGetFullDisplayName()
                    printfn "FullTypeSafe -> %A" m.FullTypeSafe
                    // let v = m.
                    // v.
                    // printfn "%A"
                    let name =
                        if m.DeclaringEntity.IsSome then
                            String.Join(".", m.DeclaringEntity.Value.FullName, m.DisplayName)
                        else
                            m.FullTypeSafe.Value.Format(FSharpDisplayContext.Empty)
                    // printfn "name -> %s" name

                    match
                        matchers
                        |> Seq.tryFind(fun k -> name.Contains(k.Key))
                        with
                    | Some v ->
                        state.Add (v.Value, range)
                    | _ -> ()
                // printfn "ctx.TypedTree.Declarations --> %A" ctx.TypedTree.Declarations
                ctx.TypedTree.Declarations |> List.iter (visitDeclaration handler)
                state
                |> Seq.map (fun (hash, r) ->
                    { Type = "Weak hashing analyzer"
                      Message = sprintf "%A shouldn't be used.  Consider changing to SHA256 or SHA512." hash
                      Code = toCode hash
                      Severity = Warning
                      Range = r
                      Fixes = []}

                )
                |> Seq.toList
