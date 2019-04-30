module FP.Task03.Program

open FP.Task03.CsvPointsReader
open FP.Task03.LagrangeInterpolator
open FP.Task03.LinearSegmentsApproximator

[<EntryPoint>]
let main args =
  match args with
  | [| "segments"; step; |] ->
    printPoints (approximate (readPoints()) (float step))
  | [| "lagrange"; step; minAmountOfPoints |] ->
    printPoints (interpolate (readPoints()) (float step) (int minAmountOfPoints))
  | _ ->
    printfn "%s" (
      "Usage: (type: lagrange | segments) (step : float)\n" +
      "Example: lagrange 0.01\n"
    )

  0


