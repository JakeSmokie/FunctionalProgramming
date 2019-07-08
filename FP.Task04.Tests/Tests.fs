open Expecto
open FP.Task04.StateMachine
open FP.Task04.TrafficLight
open FP.Task04.Regex

let testTFParameters = {
  GreenTicks = 5
  BlinkingGreenTicks = 3
  ShortenedGreenTicks = 1
  YellowTicks = 3
  RedTicks = 3
 }

let tf = createTrafficLight testTFParameters
let getStates = List.map (fun x -> x.CurrentState)

[<Tests>]
let tests =
  testList "All" [
    testList "TF Tests" [
      test "TF loop works" {
        let actual = tf :: iterateTimes tf 30 |> getStates

        let expected = [
          Initial;
          Green; Green; Green; Green; Green;
          BlinkingGreen; BlinkingGreen; BlinkingGreen;
          YellowToRed; YellowToRed; YellowToRed;
          Red; Red; Red;
          YellowToGreen; YellowToGreen; YellowToGreen;
          Green; Green; Green; Green; Green;
          BlinkingGreen; BlinkingGreen; BlinkingGreen;
          YellowToRed; YellowToRed; YellowToRed;
          Red; Red
        ]

        Expect.equal actual expected ""
      }

      test "TF pedestrian button works" {
        let a = tf :: iterateTimes tf 1
        let b = a @ [ pressButton (List.last a) ]
        let actual = b @ iterateTimes (List.last b) 5

        let aStates = [ Initial; Green ]
        let bStates = aStates @ [ Green ]
        let expected = bStates @ [ Green; BlinkingGreen; BlinkingGreen; BlinkingGreen; YellowToRed; ]

        Expect.equal (a |> getStates) aStates ""
        Expect.equal (b |> getStates) bStates ""
        Expect.equal (actual |> getStates) expected ""
      }
    ]
    
    testList "Regex tests" [
      test "" {
        let a = createRegex "abcde" |> iterateUntilStates [ Success; Failure ]
        Expect.equal (List.last a).CurrentState Success ""
      }
    ]
  ]

[<EntryPoint>]
let main args =
  runTestsInAssemblyWithCLIArgs [] args
