module FP.Task02.Program

open FP.Task02.BinarySearchTree
open FSharpPlus
open System

[<EntryPoint>]
let main argv =
  let tree =
    Empty
    |> insert 3
    |> insert 2
    |> insert 4

  let mapped =
    tree
    |> map (fun x -> x * 10)
  
  
  0
