module FP.Task03.Program

open FP.Task03.CsvPointsReader
open FP.Task03.LagrangeInterpolator
open FP.Task03.LinearSegmentsApproximator

[<EntryPoint>]
let main args =
  let points = readPoints()

  match args with
  | [| "segments"; step; |] ->
    approximate points (float step) |> printPoints

  | [| "lagrange"; step; chunkSize; cutoff |] ->
    interpolate (int chunkSize) (float cutoff) points (float step) |> printPoints

  | [| "both"; step; chunkSize; cutoff |] ->
    seq {
      yield! interpolate (int chunkSize) (float cutoff) points (float step);
      yield! approximate points (float step)
    } |> printPoints

  | _ ->
    eprintfn "%s" (
      "Usage: (type: lagrange | segments | both) (step : float) (chunkSize : uint) (cutoff : [0.0..1.0) float)\n" +
      "Example: lagrange 0.01 5\n"
    )

  0
