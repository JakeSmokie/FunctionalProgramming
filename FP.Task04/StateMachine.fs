module FP.Task04.StateMachine
open System
open System
open System
open System
open System

type ParametrizedPermit<'state, 'trigger, 'func> = {
  From : 'state
  To : 'state
  Trigger : 'trigger
  Function : 'func
 }

type Permit<'state, 'trigger, 'event> = {
  From : 'state
  To : 'state
  Trigger : 'trigger
  Event : 'event
 }

type Action<'trigger, 'func> = {
  Trigger : 'trigger
  Function : 'func
 }

type StateMachine<'state, 'trigger, 'model> = {
  CurrentState : 'state
  Model : 'model
  Permits : Permit<'state, 'trigger, 'model -> 'model> list
  ParametrizedPermits : ParametrizedPermit<'state, 'trigger, Object -> 'model -> 'model> list
  Actions : Action<'trigger, Object -> 'model -> 'state -> ('model * 'state)> list
 }

let (=>) firstState secondState =
  firstState, secondState

let (<!>) (firstState, secondState) trigger = {
  From = firstState
  To = secondState
  Trigger = trigger
  Event = id
 }

let (<?>) (permit : Permit<'state, 'trigger, 'event>) func = {
  From = permit.From
  To = permit.To
  Trigger = permit.Trigger
  Function = func
 }

let (<*>) (permit : Permit<'state, 'trigger, 'event>) handler =
  { permit with Event = handler }

let (<!!>) trigger func = {
  Trigger = trigger
  Function = func
 }

let fire trigger fsm =
  match fsm.Permits |> List.tryFind (fun x -> x.Trigger = trigger && fsm.CurrentState = x.From) with
  | Some permit -> {
      fsm with CurrentState = permit.To
               Model = permit.Event fsm.Model
    }
  | None ->
    failwith "No action found"

let fireWithArg trigger param (fsm : StateMachine<'state, 'trigger, 'model>) =
  match fsm.ParametrizedPermits |> List.tryFind (fun x -> x.Trigger = trigger && fsm.CurrentState = x.From) with
  | Some permit -> {
      fsm with CurrentState = permit.To;
               Model = permit.Function param fsm.Model
    }
  | None -> failwith "No action found"
  
let fireAction trigger param (fsm : StateMachine<'state, 'trigger, 'model>) =
  match fsm.Actions |> List.tryFind (fun x -> x.Trigger = trigger) with
  | Some action ->
    let (model, state) = action.Function param fsm.Model fsm.CurrentState
    { fsm with CurrentState = state; Model = model }
  | None -> failwith "No action found"

let (/>) fsm trigger =
  fire trigger (List.head fsm) :: fsm

let (/*>) fsm (trigger, param) =
  fireWithArg trigger param (List.head fsm) :: fsm

let (/!>) fsm (trigger, param) =
  fireAction trigger param (List.head fsm) :: fsm

