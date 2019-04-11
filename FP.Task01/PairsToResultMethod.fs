module FP.Task01.PairsToResultMethod
open FSharp.Collections.ParallelSeq

let rec reverseInt' n acc =
    match n with
    | 0 -> acc
    | _ -> reverseInt' (n / 10) (acc * 10 + (n % 10))

let reverseInt n =
  reverseInt' n 0

let isPalindrome x =
  //let s = x |> string
  //s = rev s
  x = reverseInt x

let getResult n =
  let e = int (10.0 ** float n) - 1

  [1 .. e]
  |> PSeq.collect (fun x -> [x .. e] |> PSeq.map(fun y -> x * y))
  |> PSeq.filter isPalindrome
  |> PSeq.max