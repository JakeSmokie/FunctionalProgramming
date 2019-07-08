module FP.Task04.Regex
open FP.Task04.StateMachine

type RegexState =
  | Init
  | A
  | B
  | Success
  | Failure

type RegexModel = {
  Input : string
  Cursor : int
 }

let inc m = { m with Cursor = m.Cursor + 1 }
let checkCurrentChar c m = m.Input.[m.Cursor] = c

let createRegex input = {
  CurrentState = Init
  Model = {
    Input = input
    Cursor = 0
  }

  Permits = [
    Init, A, checkCurrentChar 'a', inc
    Init, Failure, pTrue, id

    A, B, checkCurrentChar 'b', inc
    A, Failure, pTrue, id

    B, Success, pTrue, inc
  ]

  IgnoredPermits = []
 }
