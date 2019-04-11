module FP.Task01.Main

open CS.Task01
open System

[<EntryPoint>]
let main _ =
  [2 .. 10]
  |> List.iter (fun n -> 
    if n < 4 then printfn "Loops: %d" (LoopsMethod.getResult n)
    if n < 5 then printfn "PairsToResult: %d" (PairsToResultMethod.getResult n)
    if n < 5 then printfn "ResultToPairsParsing: %d" (ResultToPairsParsingMethod.getResult n)
    printfn "Sharp: %d" (SharpMethod.GetResult n)
  )

  Console.ReadLine() |> ignore
  0