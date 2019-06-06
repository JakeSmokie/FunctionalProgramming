open FP.Task04.StateMachine
open FP.Task04.Phone

[<EntryPoint>]
let main argv =
  let a =
    [phone]
    /> CallDialed
    /*> (CallConnected, "88005553535")
    /!> (SetVolume, 100)
    /> PlacedOnHold
    /!> (SetVolume, 600)
    /!> (Destroy, "")
    /> PhoneHurledAgainstWall

  a
  |> List.map (fun x -> x.Model, x.CurrentState)
  |> List.rev
  |> List.iter (printfn "%A")
  
  0
