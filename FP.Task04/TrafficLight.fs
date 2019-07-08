module FP.Task04.TrafficLight
open System
open FP.Task04.StateMachine

type TrafficLightStates =
  | Initial
  | Green
  | Red
  | BlinkingGreen
  | YellowToRed
  | YellowToGreen
  | End

type TrafficLightModel = {
  Ticks : int
  ButtonPressed : bool
  ShouldBeDisabled : bool
 }

type TrafficLightParameters = {
  GreenTicks : int
  BlinkingGreenTicks : int
  ShortenedGreenTicks : int
  YellowTicks : int
  RedTicks : int
 }

let setTicks time m = { m with Ticks = time }
let turnOffButton m = { m with ButtonPressed = false }

let ticksEnd m = m.Ticks <= 1
let notTicksEnd m = m.Ticks > 1

let tick m = { m with Ticks = m.Ticks - 1 }

let createTrafficLight p = {
  CurrentState = Initial
  Model = {
    Ticks = 0
    ButtonPressed = false
    ShouldBeDisabled = false
  }

  Permits = [
    Initial, Green, pTrue, setTicks p.GreenTicks

    Green, End, (fun m -> m.ShouldBeDisabled), id
    End, Initial, (fun m -> not m.ShouldBeDisabled), id

    Green, Green, (fun m -> m.ButtonPressed), (fun m ->
      let ticks = p.ShortenedGreenTicks

      { ButtonPressed = false
        Ticks = if ticks > m.Ticks then m.Ticks else ticks
        ShouldBeDisabled = false }
    )

    Green, Green, notTicksEnd, tick
    Green, BlinkingGreen, ticksEnd, setTicks p.BlinkingGreenTicks

    BlinkingGreen, BlinkingGreen, notTicksEnd, tick
    BlinkingGreen, YellowToRed, ticksEnd, setTicks p.YellowTicks

    YellowToRed, YellowToRed, notTicksEnd, tick
    YellowToRed, Red, ticksEnd, setTicks p.RedTicks

    Red, Red, notTicksEnd, tick
    Red, YellowToGreen, ticksEnd, setTicks p.YellowTicks

    YellowToGreen, YellowToGreen, notTicksEnd, tick
    YellowToGreen, Green, ticksEnd, (setTicks p.GreenTicks >> turnOffButton)
  ]

  IgnoredPermits = [
    Red, Green, pTrue, id
    Red, YellowToGreen, pTrue, id
  ]
 }

let pressButton sm = { sm with Model = { sm.Model with ButtonPressed = true } }

let defaultTFParameters = {
  GreenTicks = 55
  BlinkingGreenTicks = 4
  ShortenedGreenTicks = 15
  YellowTicks = 1
  RedTicks = 10
 }
