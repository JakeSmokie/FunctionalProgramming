module FP.Task03.LinearSegmentsApproximator
open FP.Task03.FunctionValuesGenerator

let approximate points step =
  let render ((ax, ay), (bx, by)) =
    let func t =
      ay + (t - ax) * (by - ay) / (bx - ax)

    genValues func ax bx step

  points
  |> Seq.pairwise
  |> Seq.map render
  |> Seq.concat
