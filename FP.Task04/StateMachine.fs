module FP.Task04.StateMachine
open System

type Action<'trigger, 'func> = {
  Trigger : 'trigger
  Function : 'func
  Ignored : bool
 }

type Permit<'state, 'trigger, 'func> = {
  From : 'state
  To : 'state
  Action : Action<'trigger, 'func>
 }

type StateMachine<'state, 'trigger, 'model> = {
  CurrentState : 'state
  Model : 'model
  Permits : Permit<'state, 'trigger, 'model -> 'model> list
  ParametrizedPermits : Permit<'state, 'trigger, Object -> 'model -> 'model> list
  Actions : Action<'trigger, Object -> 'model -> 'state -> ('model * 'state)> list
 }

let inline private findPermit trigger fsm permits =
  let permits =
    permits
    |> List.filter (fun x -> not x.Action.Ignored && x.Action.Trigger = trigger)

  if (permits.Length = 0) then
    failwith "No action found"

  let permits =
    permits |> List.filter (fun x -> x.From = fsm.CurrentState)

  match permits with
  | [ p ] -> p
  | [] -> failwithf "No transition from state %A and trigger %A found" fsm.CurrentState trigger
  | _ -> failwithf "Too many transitions found for trigger %A" trigger

let fire trigger fsm =
  let p = findPermit trigger fsm fsm.Permits

  { fsm with CurrentState = p.To
             Model = p.Action.Function fsm.Model }

let fireWithArg trigger param fsm =
  let p = findPermit trigger fsm fsm.ParametrizedPermits

  { fsm with CurrentState = p.To
             Model = p.Action.Function param fsm.Model }

let fireAction trigger param fsm =
  let action =
    match fsm.Actions |> List.tryFind (fun x -> not x.Ignored && x.Trigger = trigger) with
    | Some a -> a
    | None -> failwith "No action found"

  let (model, state) = action.Function param fsm.Model fsm.CurrentState
  { fsm with CurrentState = state; Model = model }

let (=>) (firstState : 'state) (secondState : 'state) =
  firstState, secondState

let (<!>) ((firstState : 'state), (secondState : 'state)) (trigger : 'trigger) = {
  From = firstState
  To = secondState

  Action = {
    Trigger = trigger
    Function = id
    Ignored = false
  }
 }

let (<?>) (permit : Permit<'state, 'trigger, 'event>) (func : Object -> 'model -> 'model) = {
  From = permit.From
  To = permit.To

  Action = {
    Trigger = permit.Action.Trigger
    Function = func
    Ignored = false
  }
 }

let (<!!>) (trigger : 'trigger) (func : Object -> 'model -> 'state -> ('model * 'state)) = {
  Trigger = trigger
  Function = func
  Ignored = false
 }

let (<*>) (permit : Permit<'state, 'trigger, 'event>) handler = {
  permit with Action = { permit.Action with Function = handler }
 }

let (/>) fsm trigger =
  fire trigger (List.head fsm) :: fsm

let (/*>) fsm (trigger, param) =
  fireWithArg trigger param (List.head fsm) :: fsm

let (/!>) fsm (trigger, param) =
  fireAction trigger param (List.head fsm) :: fsm

let asDot name (fsm : StateMachine<'state, 'trigger, 'model>) =
  printfn "digraph %s {" name

  fsm.Permits |> List.iter (fun p ->
    printfn "  %A -> %A [label=%A]" p.From p.To p.Action.Trigger
  )

  fsm.ParametrizedPermits |> List.iter (fun p ->
    printfn "  %A -> %A [label=%A]" p.From p.To p.Action.Trigger
  )

  printfn ""

  let ignored = List.concat [
    fsm.Permits |> List.filter (fun x -> x.Action.Ignored) |> List.map (fun x -> (x.From, x.To))
    fsm.ParametrizedPermits |> List.filter (fun x -> x.Action.Ignored) |> List.map (fun x -> (x.From, x.To))
  ]

  ignored |> List.iter (fun (a, b) ->
    printfn "  %A -> %A [style=dotted, arrowhead=none]" a b
  )

  printfn "\n  subgraph actions { "
  printfn "    label=\"actions\" \n"

  fsm.Actions |> List.iter (fun p ->
    printfn "    %A [shape=\"box\"]" p.Trigger
  )

  printfn "  }\n"
  printfn "}\n"

let statesAsDot (states : StateMachine<'state, 'trigger, 'model> list) =
  printfn "digraph states {"

  states |> Seq.iteri (fun i s ->
    printfn "  %A [label=\"%A\"]" i s.CurrentState
  )

  printfn ""

  states |> Seq.pairwise |> Seq.iteri (fun i _ ->
    printfn "  %A -> %A" i (i + 1)
  )

  printfn "}\n"

let ignoredPermit (permit : Permit<_, _, _>) =
  { permit with Action = { permit.Action with Ignored = true } }

let ignoredAction (permit : Action<_, _>) =
  { permit with Ignored = true }
