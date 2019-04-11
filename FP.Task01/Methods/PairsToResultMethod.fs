namespace FSTasks.Task01
open FSharp.Collections.ParallelSeq

type PairsToResultMethod() =
    interface ILargerPalindromicNumberFinder with
        member this.GetResult() =
            let rec merge xs =
                match xs with 
                | [] -> 0
                | x :: xs -> x + merge (xs |> List.map (fun x -> x * 10))

            let rec makeList x = 
                match x with
                | 0 -> []
                | _ -> (x % 10) :: (makeList (x / 10))

            let isPalindrome x =
                x = merge (makeList x |> List.rev)

            [100 .. 999]
            |> PSeq.collect (fun x -> [x .. 999] |> PSeq.map(fun y -> x * y))
            |> PSeq.filter isPalindrome
            |> PSeq.max