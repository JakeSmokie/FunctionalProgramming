module FP.Task03.FunctionValuesGenerator

let saveRes f x = (x, f x)

let genValues f a b step =
  let n = (b - a) / step
  let xs = List.map (fun n -> a + step * float n) [ 0..int (ceil n) ]

  List.map (saveRes f) xs
