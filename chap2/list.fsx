
let rec (++) xs ys =
  match xs with
    | ( head :: tail ) -> head :: (tail ++ ys)
    | _ -> ys

let rec update x i y =
    match (x, i, y) with
        | (x :: xs, 0, y) -> y :: xs
        | (x :: xs, i, y) -> x :: update xs (i - 1) y

let rec suffixes l =
    match l with
        | (x :: xs) -> l :: suffixes xs
        | _ -> [[]]




let x = 0 :: 1 :: 2 :: []
let y = 3 :: 4 :: 5 :: []
let z = x ++ y

suffixes (1 :: 2 :: 3 :: 4 :: [])


