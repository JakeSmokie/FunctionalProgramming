module Task1

[<EntryPoint>]
let main _ =
    let methods : ILargerPalindromicNumberFinder list = [
        PairsToResultMethod();
        ResultToPairsParsingMethod()
    ]

    methods
    |> List.map (fun x -> x.GetResult())
    |> List.iter (printfn "%d")

    0