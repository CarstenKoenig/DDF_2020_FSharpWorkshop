module Combinatorics

// Hilfsfunktionen für Tag 7
// schamlos aus StackOverflow geklaut
// https://stackoverflow.com/a/3129136/76051

let rec private distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)
