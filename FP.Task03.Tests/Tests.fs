module FP.Task03.Tests.Tests

open Expecto
open FP.Task03.LagrangeInterpolator
open FP.Task03.LinearSegmentsApproximator
open FP.Task03.FunctionValuesGenerator

let joinByX a b =
  Seq.allPairs a b
  |> Seq.where (fun ((ax, ay), (bx, by)) -> abs (bx - ax) < 0.000001)

let validate app f a b step checkStep avgError peakError =
  let interpolationPoints = gen f a b step
  let expected = gen f a b checkStep
  let actual = app interpolationPoints checkStep

  let errors =
    joinByX actual expected
    |> Seq.map (fun ((ax, ay), (bx, by)) -> abs (by - ay))
    |> Seq.cache

  Expect.isLessThan (Seq.average errors) avgError "Avg error is too high"
  Expect.all errors ((>) peakError) "Peak error is too high"

let testLagrange f a b step checkStep chunkSize cutoff =
  validate (interpolate chunkSize cutoff) f a b step checkStep

[<Tests>]
let tests =
  testList "Approximation Tests" [
    test "Sin -> Lagrange" {
      let test = testLagrange sin -30.14 30.14 1.0 0.1
      let peak = 0.5

      // chunkSize cutoff avgError peakError
      test 4 0.125 0.1 peak
      test 4 0.25 0.1 peak
      test 5 0.25 0.025 peak
      test 6 0.125 0.02 peak
      test 10 0.125 0.02 peak
      test 20 0.125 0.02 peak
    }

    test "Line -> Lagrange" {
      let test = testLagrange ((+) 10.0) -10.0 10.0 1.0 0.1
      let (avg, peak) = (0.00000001, 0.0001)

      test 2 0.0 avg peak
      test 5 0.0 avg peak
      test 10 0.0 avg peak
      test 20 0.01 avg peak
    }

    test "Wavy -> Lagrange" {
      let test = testLagrange (sin >> cos >> (*) 10.0) -10.0 10.0 0.5 0.1
      let peak = 0.1

      test 7 0.25 0.0166 peak
      test 8 0.25 0.0191 peak
      test 9 0.25 0.00817 peak
      test 10 0.25 0.007844 peak
      test 20 0.25 0.007650 peak
    }

    test "Functions -> LSA" {
      let test f = validate approximate f -10.0 10.0 0.5 0.1

      test sin 0.0102 0.03
      test ((+) 10.0) 0.00000001 0.00000001
      test (sin >> cos >> (*) 10.0) 0.098 0.3
    }
  ]

[<EntryPoint>]
let main args =
  runTestsInAssemblyWithCLIArgs [] args
