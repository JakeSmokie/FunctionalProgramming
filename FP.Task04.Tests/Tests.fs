open Expecto
open FP.Task04.Phone
open FP.Task04.Archvile
open FP.Task04.StateMachine

[<Tests>]
let tests =
  testList "FSM Tests" [
    test "Phone test" {
      let phone =
        phone
        /*> (CallDialed, "88005553535")
        /> CallConnected
        /!> (SetVolume, 100)
        /> PlacedOnHold
        /> TakenOffHold
        /> CallEnded
        |> List.rev
      
      let expectedStates = [
        OffHook
        Ringing
        Connected
        Connected
        OnHold
        Connected
        OffHook
      ]
      
      let actualStates = phone |> List.map (fun s -> s.CurrentState)
      Expect.equal actualStates expectedStates "States are ok"
    }
    
    test "Archvile Test" {
      let archvile =
        archvile
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
        
      let expectedStates = [
        Waiting;
        Chasing;
        MakingFire;
        Waiting;
        Healing;
        InPain;
        Waiting;
        Healing;
        Waiting;
        Waiting;
        Waiting;
        MakingFire;
        Waiting;
        Chasing;
        Waiting;
        Gibbed;
        Gibbed
      ]
      
      
      let actualStates = archvile |> List.map (fun s -> s.CurrentState)
      let actualModel = (List.last archvile).Model
       
      Expect.equal actualStates expectedStates "Arch is behaving ok"
      Expect.equal actualModel.Health (-9750) "Arch is damaged well"
      Expect.equal actualModel.RevivedAmount 1 "Arch revived someone"
    }
  ]

[<EntryPoint>]
let main args =
  runTestsInAssemblyWithCLIArgs [] args
