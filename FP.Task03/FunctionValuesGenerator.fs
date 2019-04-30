module FP.Task03.FunctionValuesGenerator

let saveRes f x = (x, f x)

let genValues f a b step =
  let (l, r) = (min a b, max a b)

  let n = (r - l) / step
  let xs = Seq.map (fun n -> l + step * float n) [ 0..int (ceil n) - 1 ]

  Seq.map (saveRes f) xs
