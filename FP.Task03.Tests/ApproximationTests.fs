module FP.Task03.Tests.ApproximationTests

open FP.Task03.LagrangeInterpolator
open FP.Task03.LinearSegmentsApproximator
open FP.Task03.FunctionValuesGenerator
open FsUnit
open Xunit

let validateOnRange app f a b step checkStep maxError =
  let interpolationPoints = genValues f a b step
  let expected = Seq.toList(genValues f a b checkStep)
  let actual = Seq.toList(app interpolationPoints checkStep)

  let checkError (ax, ay) (bx, by) =
    abs (by - ay) |> should lessThan maxError
    abs (bx - ax) |> should lessThan 0.0000000001
    
  actual.Length |> should equal expected.Length
  List.iter2 checkError expected actual

[<Fact>]
let ``functions are interpolated correctly``() =
 //  validateOnRange interpolate sin -10.0 10.0 0.5 0.1 0.000001
//  validateOnRange interpolate ((+) 10.0) -10.0 10.0 0.5 0.1 0.00002
//  validateOnRange interpolate (sin >> cos >> (*) 10.0) -10.0 10.0 0.1 0.1 0.000001
//  validateOnRange approximate sin -10.0 10.0 0.5 0.1 0.1
//  validateOnRange approximate ((+) 10.0) -10.0 10.0 0.5 0.1 0.00000001
  validateOnRange approximate (sin >> cos >> (*) 10.0) -10.0 10.0 0.5 0.1 0.3
