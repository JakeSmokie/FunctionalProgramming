module FP.Task04.Phone
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
  | LeftMessage
  | PlacedOnHold
  | TakenOffHold
  | PhoneHurledAgainstWall
  | MuteMicrophone
  | UnmuteMicrophone
  | SetVolume
  | Destroy

type PhoneModel = {
  Volume : int
  Callee : string
 }

let phone = {
  CurrentState = OffHook
  Model = {
    Volume = 0
    Callee = ""
  }

  Permits = [
    OffHook => Ringing <!> CallDialed
    Connected => OffHook <!> LeftMessage
    Connected => OnHold <!> PlacedOnHold
    OnHold => Connected <!> TakenOffHold
    OnHold => PhoneDestroyed <!> PhoneHurledAgainstWall
  ]

  ParametrizedPermits = [
    Ringing => Connected <!> CallConnected <?> (fun x model -> { model with Callee = (x :?> string) })
  ]

  Actions = [
    SetVolume <!!> (fun x model state -> ({ model with Volume = x :?> int }, state))
    Destroy <!!> (fun x model state -> (model, PhoneDestroyed))
  ]
 }
