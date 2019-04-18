module FP.Task03.Program

open XPlot.GoogleCharts
open FP.Task03.CsvPointsReader
open FP.Task03.LagrangeInterpolator
open FP.Task03.LinearSegmentsApproximator
open FP.Task03.FunctionValuesGenerator

let getFunc = function
  | "sin" -> sin
  | "cos" -> cos
  | "tan" -> tan
  | "sin+x" -> fun x -> sin x + x
  | "x" -> fun x -> x
  | _ -> fun x -> 0.0

let gen a b step func =
  let (a, b, step) = (float a, float b, float step)
  let points = genValues (getFunc func) a b step

  printPoints points

let getApproxFunc approxType =
  match approxType with
  | "lagrange" -> interpolateByPoints
  | "segments" -> approximateByPoints
  | _ -> failwith "Unknown approximation type. Avaliable: lagrange, segments"

let run approxType a b step render =
  let points = readPoints()
  let func = getApproxFunc approxType

  let (a, b, step) = (float a, float b, float step)
  let approxPoints = genValues (func points) a b step

  if render then
    [ approxPoints; Seq.toList (Seq.sortBy fst points) ]
    |> Chart.Line
    |> Chart.WithHeight 1080
    |> Chart.Show

  printPoints approxPoints

let fail() =
  printfn "%s" (
    "Usage: (type: lagrange | segments | gen (func)) (from : float) (to : float) (drawstep : float) [--render: flag]\n" +
    "Example: lagrange 1.0 2.0 0.1\n"
  )

[<EntryPoint>]
let main args =
  match args with
  | [| "gen"; func; a; b; step |] -> gen a b step func
  | [| approxType; a; b; step; |] -> run approxType a b step false
  | [| approxType; a; b; step; "--render" |] -> run approxType a b step true
  | _ -> fail()

  0


