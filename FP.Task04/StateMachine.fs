module FP.Task04.StateMachine
open System.Text
open Printf

let pTrue _ = true

type StateMachine<'state, 'model> = {
  CurrentState : 'state
  Model : 'model
  Permits : ('state * 'state * ('model -> bool) * ('model -> 'model)) list
  IgnoredPermits : ('state * 'state * ('model -> bool) * ('model -> 'model)) list
 } with
  member this.AsDot() =
    let sb = new StringBuilder() 
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
    |> Seq.tryFind (fun (a, b, predicate, action) -> sm.CurrentState = a && predicate sm.Model)

  match permit with
  | None -> sm
  | Some(a, b, predicate, action) ->
    { sm with CurrentState = b
              Model = action sm.Model }

let iterateTimes sm times =
  let rec iter t sm =
    match t with
    | 0 -> seq { yield! Seq.empty }
    | _ -> seq {
      let sm = iterate sm
      
      yield sm
      yield! iter (t - 1) sm
    }

  iter times sm
  |> Seq.toList 