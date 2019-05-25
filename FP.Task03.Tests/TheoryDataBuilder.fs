module FP.Task03.Tests.TheoryDataBuilder
open Xunit

type TheoryDataBuilder<'a>() =
  member __.Yield x = Seq.singleton x
  member __.Zero() = Seq.empty
  member __.Combine(x, y) = Seq.append x y
  member __.Delay x = x()
  member __.Run x =
    (new TheoryData<'a>(), x)
    ||> Seq.fold (fun y x -> y.Add x; y)

let theory = new TheoryDataBuilder<int * float>()

let test = theory {
  yield (1, 0.0)
  yield (2, 0.0)
 }

let theory2 = new TheoryDataBuilder<int * (float -> float)>()

let test2 = theory2 {
  yield (1, sin)
  yield (2, cos)
 }
