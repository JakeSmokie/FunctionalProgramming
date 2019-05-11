module FP.Task03.Tests.ApproximationTests

open FP.Task03.LagrangeInterpolator
open FP.Task03.LinearSegmentsApproximator
open FP.Task03.FunctionValuesGenerator
open FsUnit
open Xunit

let joinByX a b =
  Seq.allPairs a b
  |> Seq.where (fun ((ax, ay), (bx, by)) -> abs (bx - ax) < 0.000001)

let validateOnRange app f a b step checkStep maxError =
  let interpolationPoints = genValues f a b step
  let expected = genValues f a b checkStep
  let actual = app interpolationPoints checkStep

  let checkError ((ax, ay), (bx, by)) =
    abs (by - ay) |> should lessThan maxError
    abs (bx - ax) |> should lessThan 0.0000000001

  Seq.iter checkError (joinByX actual expected)

[<Fact>]
let ``functions are interpolated correctly``() =
  validateOnRange (interpolate 3) sin -3.14 3.14 1.0 0.1 0.1
  validateOnRange (interpolate 4) sin -3.14 3.14 1.0 0.1 0.1
  validateOnRange (interpolate 5) sin -3.14 3.14 1.0 0.1 0.02
  validateOnRange (interpolate 6) sin -3.14 3.14 1.0 0.1 0.02
  validateOnRange (interpolate 2) ((+) 10.0) -10.0 10.0 2.0 0.1 0.00002
  validateOnRange (interpolate 5) ((+) 10.0) -10.0 10.0 2.0 0.1 0.00002

  validateOnRange approximate sin -10.0 10.0 0.5 0.1 0.1
  validateOnRange approximate ((+) 10.0) -10.0 10.0 0.5 0.1 0.00000001
  validateOnRange approximate (sin >> cos >> (*) 10.0) -10.0 10.0 0.5 0.1 0.3
