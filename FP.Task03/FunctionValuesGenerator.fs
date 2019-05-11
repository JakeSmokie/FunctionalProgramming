module FP.Task03.FunctionValuesGenerator

let minmax a b = (min a b, max a b)
let saveRes f x = (x, f x)

let genValues f a b (step : float) =
  let (l, r) = minmax a b
  Seq.map (saveRes f) [ l..step..r ]
