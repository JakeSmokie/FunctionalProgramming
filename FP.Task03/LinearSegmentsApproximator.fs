module FP.Task03.LinearSegmentsApproximator
open FP.Task03.FunctionValuesGenerator

let approximate points step =
  let render ((ax, ay), (bx, by)) =
    let func t =
      ay + (t - ax) * (by - ay) / (bx - ax)
    
    genValues func ax bx step

  Seq.map render (Seq.pairwise points)
  |> Seq.concat
  
  // Pairwise example:
  //   Seq.pairwise [1..5]
  //   val it : seq<int * int> = seq [(1, 2); (2, 3); (3, 4); (4, 5)]