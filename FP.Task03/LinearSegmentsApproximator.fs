module FP.Task03.LinearSegmentsApproximator
open FSharp.Collections.ParallelSeq

let rec findRange t xs =
  match xs with
  | (ax, ay) :: (bx, by) :: tail
    when t >= ax && t <= bx -> ((ax, ay), (bx, by))
  | a :: b :: tail -> findRange t (b :: tail)
  | _ -> failwith "Cannot find suitable range"

let approximateByPoints (points : list<float * float>) t =
  let rangeBounds =
    PSeq.distinctBy (fun (x, y) -> x) points
    |> PSeq.sortBy (fun (x, y) -> x)
    |> PSeq.toList
  
  let (min, _) = rangeBounds.Head
  let (max, _) = List.last rangeBounds
  
  let ((ax, ay), (bx, by)) =
    findRange t rangeBounds

  ay + (t - ax) * (by - ay) / (bx - ax)