module FP.Task03.Tests.ApproximationTests

open FP.Task03.LagrangeInterpolator
open FP.Task03.LinearSegmentsApproximator
open FP.Task03.FunctionValuesGenerator
open FsUnit
open Xunit

let validateOnRange app f a b step checkStep maxError =
  let interpolationPoints = genValues f a b step
  let expected = genValues f a b checkStep
  
  let lagrange = app interpolationPoints
  let actual = genValues lagrange a b checkStep    
  
  let checkError (ax, ay) (bx, by) =
    abs (by - ay) |> should lessThan maxError
    abs (bx - ax) |> should lessThan 0.0000000001

  List.iter2 checkError expected actual

[<Fact>]
let ``functions are interpolated correctly``() =
  validateOnRange interpolateByPoints sin -10.0 10.0 0.5 0.1 0.000001
  validateOnRange interpolateByPoints ((+) 10.0) -10.0 10.0 0.5 0.1 0.00001
  validateOnRange interpolateByPoints (sin >> cos >> (*) 10.0) -10.0 10.0 0.1 0.1 0.000001
  
  validateOnRange approximateByPoints sin -10.0 10.0 0.5 0.1 0.1
  validateOnRange approximateByPoints ((+) 10.0) -10.0 10.0 0.5 0.1 0.00000001
  validateOnRange approximateByPoints (sin >> cos >> (*) 10.0) -10.0 10.0 0.25 0.1 0.1