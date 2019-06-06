module FP.Task04.StateMachine
open System
open System
open System
open System
open System

type ParametrizedPermit<'a, 'b, 'c> = {
  From : 'a
  To : 'a
  Trigger : 'b
  Function : 'c
 }

type Permit<'a, 'b> = {
  From : 'a
  To : 'a
  Trigger : 'b
 }

type Action<'a, 'b> = {
  Trigger : 'a
  Function : 'b
 }

type StateMachine<'state, 'trigger, 'model> = {
  CurrentState : 'state
  Model : 'model
  Permits : Permit<'state, 'trigger> list
  ParametrizedPermits : ParametrizedPermit<'state, 'trigger, Object -> 'model -> 'model> list
  Actions : Action<'trigger, Object -> 'model -> 'state -> ('model * 'state)> list
 }

let (=>) firstState secondState =
  firstState, secondState

let (<!>) (firstState, secondState) trigger = {
  From = firstState
  To = secondState
  Trigger = trigger
 }

let (<?>) (permit : Permit<'a, 'b>) func = {
  From = permit.From
  To = permit.To
  Trigger = permit.Trigger
  Function = func
 }

let (<!!>) trigger func = {
  Trigger = trigger
  Function = func
 }

let fire trigger fsm =
  match fsm.Permits |> List.tryFind (fun x -> x.Trigger = trigger) with
  | Some permit when fsm.CurrentState <> permit.From -> fsm // failwithf "No transition found: %A %A" permit fsm.CurrentState 
  | Some permit -> {
      fsm with CurrentState = permit.To
    }
  | None -> fsm // failwith "No action found"

let fireWithArg trigger param (fsm : StateMachine<'state, 'trigger, 'model>) =
  match fsm.ParametrizedPermits |> List.tryFind (fun x -> x.Trigger = trigger) with
  | Some permit when fsm.CurrentState <> permit.From -> fsm // failwithf "No transition found: %A %A" permit fsm.CurrentState
  | Some permit -> {
      fsm with CurrentState = permit.To;
               Model = permit.Function param fsm.Model
    }
  | None -> fsm // failwith "No action found"
  
let fireAction trigger param (fsm : StateMachine<'state, 'trigger, 'model>) =
  match fsm.Actions |> List.tryFind (fun x -> x.Trigger = trigger) with
  | Some action ->
    let (model, state) = action.Function param fsm.Model fsm.CurrentState
    { fsm with CurrentState = state; Model = model }
  | None -> fsm // failwith "No action found"

let (/>) fsm trigger =
  fire trigger (List.head fsm) :: fsm

let (/*>) fsm (trigger, param) =
  fireWithArg trigger param (List.head fsm) :: fsm

let (/!>) fsm (trigger, param) =
  fireAction trigger param (List.head fsm) :: fsm

