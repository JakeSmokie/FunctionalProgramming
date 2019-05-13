module FP.Task03.LagrangeInterpolator
open FP.Task03.FunctionValuesGenerator

let lagrange points x0 =
  let multipliers ax =
    Seq.filter (fun (x, _) -> x <> ax) points
    |> Seq.fold (fun acc (x, _) -> acc * (x0 - x) / (ax - x)) 1.0

  points
  |> Seq.map (fun (x, y) -> y * multipliers x)
  |> Seq.sum

let gen points a b step =
  genValues (lagrange points) (fst a) (fst b) step

let windowedWithStep chunkSize step seq =
  [ 0..Seq.length seq / step ]
  |> Seq.map (fun i -> Seq.skip (i * step) seq |> Seq.truncate chunkSize)
  |> Seq.filter (fun seq -> Seq.length seq = chunkSize)

let interpolate chunkSize points step =
  let genSegment chunk =
    let left = Seq.item (chunkSize / 4) chunk
    let right = Seq.item (chunkSize * 3 / 4) chunk
    
    gen chunk left right step

  points
  |> windowedWithStep chunkSize (chunkSize / 2)
  |> Seq.map genSegment
  |> Seq.concat

