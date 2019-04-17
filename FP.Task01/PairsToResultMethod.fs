module FP.Task01.PairsToResultMethod
open FSharp.Collections.ParallelSeq

let rec reverseInt' acc = function
  | 0 -> acc
  | n -> reverseInt' (n / 10) (acc * 10 + (n % 10))

let reverseInt n =
  reverseInt' 0 n

let isPalindrome x =
  x = reverseInt x

let getResult n =
  let e = (pown 10 n) - 1

  List.allPairs [ 1..e ] [ 1..e ]
  |> PSeq.map (fun (x, y) -> x * y)
  |> PSeq.filter isPalindrome
  |> PSeq.max
