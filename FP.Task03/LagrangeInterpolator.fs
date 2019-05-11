module FP.Task03.LagrangeInterpolator
open FP.Task03.FunctionValuesGenerator

let lagrange points t =
  let multipliers a =
    Seq.filter (fun (x, _) -> x <> a) points
    |> Seq.fold (fun acc (x, _) -> acc * (t - x) / (a - x)) 1.0

  points
  |> Seq.map (fun (x, y) -> y * multipliers x)
  |> Seq.sum

let gen points a b step =
  genValues (lagrange points) (fst a) (fst b) step

let interpolate minAmountOfPoints points step =
  let start = Seq.take minAmountOfPoints points

  let genSegment i pair =
    let segment =
      Seq.take (minAmountOfPoints + i + 1) points

    gen segment (fst pair) (snd pair) step

  seq {
    yield! (gen start (Seq.head start) (Seq.last start) step)
    yield! (points
      |> Seq.skip (minAmountOfPoints - 1)
      |> Seq.pairwise
      |> Seq.mapi genSegment
      |> Seq.concat
    )
  }
