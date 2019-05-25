module FP.Task03.LagrangeInterpolator
open FP.Task03.FunctionValuesGenerator

let lagrange points x0 =
  let multipliers ax =
    points
    |> Seq.filter (fun (bx, by) -> bx <> ax)
    |> Seq.fold (fun acc (bx, by) -> acc * (x0 - bx) / (ax - bx)) 1.0

  points
  |> Seq.sumBy (fun (ax, ay) -> ay * multipliers ax)

let trySkip offset seq =
  match Seq.tryItem offset seq with
  | None -> Seq.empty
  | Some _ -> Seq.skip offset seq

let chunkWithStep chunkSize step points =
  let chunk offset =
    points
    |> trySkip offset
    |> Seq.truncate chunkSize

  Seq.initInfinite ((*) step)
  |> Seq.map chunk
  |> Seq.takeWhile (not << Seq.isEmpty)

let interpolate chunkSize cutoff points step =
  let cut chunk =
    let (ax, _) = Seq.head chunk
    let (bx, _) = Seq.last chunk
    let length = bx - ax

    ((ax + cutoff * length), (bx - cutoff * length))

  let gen chunk =
    let (l, r) = cut chunk
    gen (lagrange chunk) l r step

  points
  |> chunkWithStep chunkSize (chunkSize / 2)
  |> Seq.collect gen
