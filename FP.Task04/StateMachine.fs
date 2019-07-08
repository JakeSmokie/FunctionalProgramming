module FP.Task04.StateMachine
open System.Text
open Printf

type StateMachine<'state, 'model> = {
  CurrentState : 'state
  Model : 'model
  Permits : ('state * 'state * ('model -> bool) * ('model -> 'model)) list
  IgnoredPermits : ('state * 'state * ('model -> bool) * ('model -> 'model)) list
 } with
  member this.AsDot() =
    let sb = StringBuilder()
    bprintf sb "digraph sm {\n"

    this.Permits |> List.iter (fun (a, b, _, _) ->
      bprintf sb "  %A -> %A\n" a b
    )

    this.IgnoredPermits |> List.iter (fun (a, b, _, _) ->
      bprintf sb "  %A -> %A [style=dotted]\n" a b
    )

    bprintf sb "}\n"
    sb.ToString()

let iterate sm =
  let permit =
    sm.Permits
    |> Seq.tryFind (fun (a, _, predicate, _) -> sm.CurrentState = a && predicate sm.Model)

  match permit with
  | None -> sm
  | Some(_, b, predicate, action) ->
    { sm with CurrentState = b
              Model = action sm.Model }

let iterateTimes sm times =
  if times = 0 then [] else
  [2..times]
  |> List.fold (fun s _ -> iterate s.Head :: s) [iterate sm]
  |> List.rev

let pTrue _ = true

let iterateUntilStates states sm =
  List.unfold (fun sm ->
    if List.contains sm.CurrentState states then None
    else Some(iterate sm, iterate sm)
  ) sm