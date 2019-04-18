module FP.Task03.LagrangeInterpolator
open FSharp.Collections.ParallelSeq

let interpolateByPoints points t =
  let multipliers a t =
    PSeq.filter (fun (x, y) -> x <> a) points
    |> PSeq.fold (fun acc (x, y) -> acc * (t - x) / (a - x)) 1.0
  
  PSeq.sum (PSeq.map (fun (x, y) -> y * multipliers x t) points) 
