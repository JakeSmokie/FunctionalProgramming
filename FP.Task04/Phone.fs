module FP.Task04.Phone
open System
open FP.Task04.StateMachine

type PhoneState =
  | OffHook
  | Ringing
  | Connected
  | OnHold
  | PhoneDestroyed

type PhoneTrigger =
  | CallDialed
  | CallConnected
  | CallEnded
  | PlacedOnHold
  | TakenOffHold
  | PhoneHurledAgainstWall
  | SetVolume
  | Destroy

type PhoneModel = {
  Volume : int
  Callee : string option
  CallStart : DateTime option
  CallEnd : DateTime option
  HoldStart : DateTime
  HoldTime : TimeSpan
 }

let phone = [ {
  CurrentState = OffHook
  Model = {
    Volume = 10
    Callee = None
    CallStart = None
    CallEnd = None
    HoldStart = DateTime.MinValue
    HoldTime = TimeSpan.Zero
  }

  Permits = [
    Ringing => Connected <!> CallConnected <*> (fun model -> { model with CallStart = Some DateTime.Now })
    Connected => OffHook <!> CallEnded <*> (fun model -> { model with CallEnd = Some DateTime.Now })

    Connected => OnHold <!> PlacedOnHold <*> (fun model -> { model with HoldStart = DateTime.Now })
    OnHold => Connected <!> TakenOffHold <*> (fun model -> { model with HoldTime = model.HoldTime + (DateTime.Now - model.HoldStart) })
    OnHold => OffHook <!> CallEnded <*> (fun model -> { model with CallEnd = Some DateTime.Now })

    OnHold => PhoneDestroyed <!> PhoneHurledAgainstWall
  ]

  ParametrizedPermits = [
    OffHook => Ringing <!> CallDialed <?> (fun x model -> { model with Callee = Some(x :?> string) })
  ]

  Actions = [
    SetVolume <!!> (fun x model state -> ({ model with Volume = x :?> int }, state))
    Destroy <!!> (fun x model state -> (model, PhoneDestroyed))
  ]
 } ]
