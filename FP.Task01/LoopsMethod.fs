module FP.Task01.LoopsMethod
open FSharpPlus

let rec merge = function
  | [] -> 0
  | x :: xs -> x + merge (xs |> map (fun x -> x * 10))

let rec makeList x =
  match x with
  | 0 -> []
  | _ -> (x % 10) :: (makeList (x / 10))

let isPalindrome x =
  x = merge (makeList x |> rev)

let getResult n =
  let mutable max = 0
  let e = int (10.0 ** float n) - 1

  for i = 0 to e do
    for j = i to e do
      let a = i * j

      if isPalindrome a && a > max then
        max <- a
      
  max