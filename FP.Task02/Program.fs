module FP.Task02.Program

open System

[<EntryPoint>]
let main argv =
  let r = Random(100)

  let first = 
    [1 .. 1000000]
    |> List.sortBy (fun _ -> r.Next())
    |> List.fold (fun tree x -> tree + x) (BinarySearchTree.Zero)

  printfn "%d" first.Min
  printfn "%d" first.Max

  let second = 
    [1000100 .. 1003000]
    |> List.sortBy (fun _ -> r.Next())
    |> List.fold (fun tree x -> tree + x) (BinarySearchTree.Zero)

  let c = first + second
  0
