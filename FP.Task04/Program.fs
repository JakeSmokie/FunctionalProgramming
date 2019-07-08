module FP.Task04.Program
open System
open System.Threading
open FP.Task04.StateMachine
open FP.Task04.TrafficLight

let random = new Random()
let rec loop sm =
  let timeLeft = sm.Model.Ticks

  printf "\r                                      \r[%A %A %A]: %A"
    sm.CurrentState sm.Model.ButtonPressed sm.Model.ShouldBeDisabled timeLeft

  let sm =
    if random.Next 10 = 1 then { sm with Model = { sm.Model with ButtonPressed = true } }
    else sm

//  let sm =
//    if random.Next 20 = 1 then { sm with Model = { sm.Model with ShouldBeDisabled = true } }
//    else sm

  Thread.Sleep 1000

  if sm.CurrentState <> End then
    loop (iterate sm)

[<EntryPoint>]
let main args =
  let sm = createTrafficLight defaultTFParameters
  loop sm

  sm.AsDot() |> printfn "%s"
  0
