module FP.Task04.Archvile
open FP.Task04.StateMachine

type ArchvileState =
  | Waiting
  | Chasing
  | MakingFire
  | Healing
  | InPain
  | Dead
  | Gibbed

type ArchvileTrigger =
  | SeenSomeone
  | ThinksAllClear
  | Attacking
  | ShootFire
  | StartedRevivingMonster
  | FinishedRevivingMonster
  | Damaged
  | PainStopped
  | Healed

type ArchvileModel = {
  Health : int
  RevivedAmount : int
 }

let MaxArchvileHP = 700

let archvile = {
  CurrentState = Waiting
  Model = {
    Health = MaxArchvileHP
    RevivedAmount = 0
  }

  Permits = [
    Waiting => Chasing <!> SeenSomeone
    Chasing => Waiting <!> ThinksAllClear

    Waiting => MakingFire <!> Attacking
    Chasing => MakingFire <!> Attacking
    MakingFire => Waiting <!> ShootFire
    
    Waiting => Healing <!> StartedRevivingMonster
    Chasing => Healing <!> StartedRevivingMonster
    Healing => Waiting <!> FinishedRevivingMonster
      <*> (fun model -> { model with RevivedAmount = model.RevivedAmount + 1 })

    InPain => Waiting <!> PainStopped    
    MakingFire => Healing <!> FinishedRevivingMonster |> ignoredPermit
  ]

  Actions = [
    Damaged <!!> (fun damage model state ->
      let damage = damage :?> int
      if damage < 0 then failwith "Damage cannot be less than zero"

      let model = { model with Health = model.Health - damage }

      let state =
        match model.Health with
        | hp when hp <= -MaxArchvileHP / 2 -> Gibbed
        | hp when hp <= 0 -> Dead
        | _ when damage > MaxArchvileHP / 4 -> InPain
        | _ -> Waiting

      (model, state)
    )

    Healed <!!> (fun hpAdded model state ->
      let hpAdded = hpAdded :?> int
      if hpAdded < 0 then failwith "You cannot harm with healing"

      let newModel = { model with Health = model.Health + hpAdded }

      match newModel.Health with
      | _ when state = Gibbed -> (model, Gibbed)
      | hp when hp > 0 -> (newModel, Waiting)
      | _ -> (newModel, Dead)
    )
  ]

  ParametrizedPermits = []
 }
