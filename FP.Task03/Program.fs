module FP.Task03.Program

open System
open FP.Task03.CsvPointsReader
open FP.Task03.LagrangeInterpolator
open FP.Task03.LinearSegmentsApproximator
open XPlot.GoogleCharts

[<EntryPoint>]
let main args =
 
//  let rand = new Random()
//  let points =
//    Seq.initInfinite float
//    |> Seq.map (fun x -> (x, x + sin x * 10.0))
//    |> Seq.take 100
//  
//  interpolate points 0.1 20
//  |> Chart.Line
//  |> Chart.WithHeight 1080
//  |> Chart.WithWidth 1900
//  |> Chart.Show
  
  let points = readPoints()

  match args with
  | [| "segments"; step; |] ->
    approximate points (float step) |> printPoints

  | [| "lagrange"; step; minAmountOfPoints |] ->
    interpolate (int minAmountOfPoints) points (float step) |> printPoints

  | [| "both"; step; minAmountOfPoints |] ->
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