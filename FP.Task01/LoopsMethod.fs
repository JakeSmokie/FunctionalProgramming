module FP.Task01.LoopsMethod
open FSharpPlus

let rec merge = function
  | [] -> 0
  | x :: xs -> x + merge (map ((*) 10) xs)

let rec makeList = function
  | 0 -> []
  | x -> (x % 10) :: (makeList (x / 10))

let isPalindrome x =
  x = merge (rev (makeList x))

let getResult n =
  let mutable max = 0
  let e = (pown 10 n) - 1

  for i = 0 to e do
    for j = i to e do
      let a = i * j

      if isPalindrome a && a > max then
        max <- a

  max
