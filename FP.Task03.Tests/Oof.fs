module FP.Task03.Tests.ApproximationTests

open FP.Task03.LagrangeInterpolator
open FP.Task03.LinearSegmentsApproximator
open FP.Task03.FunctionValuesGenerator
open FsUnit
open Xunit

let joinByX a b =
  Seq.allPairs a b
  |> Seq.where (fun ((ax, ay), (bx, by)) -> abs (bx - ax) < 0.000001)

let validateOnRange app f a b step checkStep (maxError : float) =
  let interpolationPoints = genValues f a b step
  let expected = genValues f a b checkStep
  let actual = app interpolationPoints checkStep
  
  joinByX actual expected
  |> Seq.map (fun ((ax, ay), (bx, by)) -> abs (by - ay))
  |> Seq.max
  |> should lessThan maxError
  

let sinCases() =
  (new TheoryData<int>(), [1;2;3])
  ||> Seq.fold (fun y x -> y.Add x; y)

[<Theory>]
[<MemberData("sinCases")>]
let ``Sin function test``() =

  let f = sin
  let (l, r, step, checkStep) = (-30.14, 30.14, 1.0, 0.1);

  validateOnRange (interpolate 4 0.25) f l r step checkStep 0.1
  validateOnRange (interpolate 5 0.25) f l r step checkStep 0.02
  validateOnRange (interpolate 6 0.25) f l r step checkStep 0.02
  validateOnRange (interpolate 10 0.25) f l r step checkStep 0.02

[<Fact>]
let ``Line function test``() =
  let f = (+) 10.0
  let (l, r, step, checkStep) = (-10.0, 10.0, 2.0, 0.1)

  validateOnRange (interpolate 2 0.25) f l r step checkStep 0.00002
  validateOnRange (interpolate 5 0.25) f l r step checkStep 0.00002

[<Fact>]
let ``Wavy function test``() =  
  let f = (sin >> cos >> (*) 10.0)
  let (l, r, step, checkStep) = (-10.0, 10.0, 0.5, 0.1)

  validateOnRange (interpolate 7 0.25) f l r step checkStep 0.0190
  validateOnRange (interpolate 8 0.25) f l r step checkStep 0.0191
  validateOnRange (interpolate 9 0.25) f l r step checkStep 0.00817
  validateOnRange (interpolate 10 0.25) f l r step checkStep 0.007844
  validateOnRange (interpolate 20 0.25) f l r step checkStep 0.007650

[<Fact>]
let ``Linear approximation test``() =
  let (l, r, step, checkStep) = (-10.0, 10.0, 0.5, 0.1)
  let validateApp f error =
    validateOnRange approximate f l r step checkStep error  
  
  validateApp sin 0.1
  validateApp ((+) 10.0) 0.00000001
  validateApp (sin >> cos >> (*) 10.0) 0.3
