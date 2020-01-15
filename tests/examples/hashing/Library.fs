namespace examples

module Say =
    let hello name =
        let v = (Some name).Value
        printfn "Hello %s" name
