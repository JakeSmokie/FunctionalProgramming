open FP.Task04
open FP.Task04.StateMachine
open FP.Task04.Archvile

[<EntryPoint>]
let main argv =
  [ archvile ]
  /> SeenSomeone
  /> Attacking
  /> ShootFire
  /> StartedRevivingMonster
  /!> (Damaged, 300)
  /> PainStopped
  /> StartedRevivingMonster
  /> FinishedRevivingMonster
  /!> (Damaged, 100)
  /!> (Damaged, 50)
  /> Attacking
  /> ShootFire
  /> SeenSomeone
  /> ThinksAllClear
  /!> (Damaged, 10000)
  /!> (Healed, 10700)
  |> List.rev
  |> statesAsDot

  0
