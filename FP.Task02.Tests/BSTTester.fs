module FP.Task02.Tests.BSTTester
open FP.Task02

let testNode node =
  let rec verify lo hi tree =
    match tree with
    | Empty -> true
    | Node (value, left, right) ->
      match lo, hi with
      | Some lo, _ when value < lo -> false
      | _, Some hi when value > hi -> false
      | _ ->
        let hi' = defaultArg hi value |> min value |> Some
        let lo' = defaultArg lo value |> max value |> Some

        verify lo hi' left && verify lo' hi right

  verify None None node
