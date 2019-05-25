module FP.Task03.FunctionValuesGenerator

let gen f a b (step : float) =
  let (l, r) = (min a b, max a b)
  seq {
    for x in l..step..r -> (x, f x)
  }