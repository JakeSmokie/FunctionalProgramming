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
    printPoints (interpolate (int minAmountOfPoints) (readPoints()) (float step))
  | [| "both"; step; minAmountOfPoints |] ->
    let points = readPoints()

    seq {
      yield! interpolate (int minAmountOfPoints) points (float step);
      yield! approximate points (float step)
    } |> printPoints
  | _ ->
    printfn "%s" (
      "Usage: (type: lagrange | segments | both) (step : float) (minAmountOfPoints : uint)\n" +
      "Example: lagrange 0.01 5\n"
    )

  0


