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

let interpolate points step chunkSize =
  let genSegment chunk =
    gen chunk (Seq.item (chunkSize / 4) chunk) (Seq.item (chunkSize * 3 / 4) chunk) step

  points
  |> Seq.windowed chunkSize
  |> Seq.indexed
  |> Seq.where (fun (i, _) -> i % (chunkSize / 2) = 0)
  |> Seq.map (snd >> genSegment)
  |> Seq.concat