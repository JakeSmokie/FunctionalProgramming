namespace FSTasks.Task01

type ResultToPairsParsingMethod() =
    interface ILargerPalindromicNumberFinder with
        member this.GetResult() =
            let getPalindrome x = x * 1000 + 100 * (x % 10) + ((x / 10) % 10) * 10 + x / 100

            let rec checkFactors a b = 
                match a, b with
                    |    _ , 99 -> false
                    | _ , _ when a % b = 0 && a / b < 1000 -> true
                    | _ , _ -> checkFactors a (b - 1)
 
            ([999 .. -1 .. 1] |> List.map getPalindrome |> List.find (fun x-> checkFactors x 999))