module FP.Task03.LinearSegmentsApproximator
open FP.Task03.FunctionValuesGenerator

let approximate points step =
  let render ((ax, ay), (bx, by)) =
    let line t =
      ay + (t - ax) * (by - ay) / (bx - ax)

    gen line ax bx step

  points
  |> Seq.pairwise
  |> Seq.collect render
