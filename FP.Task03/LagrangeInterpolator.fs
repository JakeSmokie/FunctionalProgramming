module FP.Task03.LagrangeInterpolator
open FP.Task03.FunctionValuesGenerator

let interpolate points step minAmountOfPoints =
  let lagrange points t =
    let multipliers a =
      Seq.filter (fun (x, y) -> x <> a) points
      |> Seq.fold (fun acc (x, y) -> acc * (t - x) / (a - x)) 1.0

    Seq.sum (Seq.map (fun (x, y) -> y * multipliers x) points)

  let gen points a b =
    genValues (lagrange points) (fst a) (fst b) step

  let rec others prevPoints = seq {
    match (Seq.tryHead points) with
    | None -> yield! (Seq.empty)
    | Some p ->
      let newPoints = Seq.append prevPoints [ p ]

      yield! gen newPoints (Seq.last prevPoints) p
      yield! others newPoints
    }

  let start =
    Seq.cache (Seq.take minAmountOfPoints points)

  seq {
    yield! (gen start (Seq.head start) (Seq.last start))
    yield! (others start)
  }
