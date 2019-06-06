open FP.Task04.StateMachine
open FP.Task04.Phone

[<EntryPoint>]
let main argv =
  let states =
    phone
    /*> (CallDialed, "8800")
    /> CallConnected
    /!> (SetVolume, 100)
    /> PlacedOnHold
    /> TakenOffHold
    /> CallEnded

  states
  |> List.map (fun x -> x.Model, x.CurrentState)
  |> List.rev
  |> List.iter (printfn "%A")
  
  0
