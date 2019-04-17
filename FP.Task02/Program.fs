module FP.Task02.Program

open FP.Task02
open System

[<EntryPoint>]
let main argv =
  let r = Random(100)

  let arr = [ 1; 2; 3 ] // List.sortBy (fun _ -> r.Next())
  let first = (Empty.AddMany arr).PerformBalance()

  printfn "%d" first.Height
  printfn "%d" first.Balance

  0
