module FP.Task03.Program

open XPlot.GoogleCharts
open FP.Task03.CsvPointsReader
open FP.Task03.LagrangeInterpolator
open FP.Task03.LinearSegmentsApproximator
open FP.Task03.FunctionValuesGenerator

let gen fileOut a b step =
  let (a, b, step) = (float a, float b, float step)
  let points = genValues (fun x -> sin x + x) a b step
  
  savePointsToCsv points fileOut

let getApproxFunc approxType =
  match approxType with
  | "lagrange" -> interpolateByPoints
  | "segments" -> approximateByPoints
  | _ -> failwith "Unknown approximation type. Avaliable: lagrange, segments"

let run fileIn fileOut approxType a b step =
  let points = getPointsOfCsv fileIn
  let func = getApproxFunc approxType

  let (a, b, step) = (float a, float b, float step)
  let approxPoints = genValues (func points) a b step

  [ approxPoints; points ]
  |> Chart.Line
  |> Chart.WithHeight 1080
  |> Chart.Show

  savePointsToCsv approxPoints fileOut

let fail() =
  failwith ("Usage: filein fileout (type: lagrange | segments) (from : float) (to : float) (drawstep : float)\n" +
            "Example: a.csv b.csv 1.0 2.0 0.1")

[<EntryPoint>]
let main args =
  match args with
  | [| "gen"; fileOut; a; b; step |] -> gen fileOut a b step
  | [| fileIn; fileOut; approxType; a; b; step |] -> run fileIn fileOut approxType a b step
  | _ -> fail()

  0


