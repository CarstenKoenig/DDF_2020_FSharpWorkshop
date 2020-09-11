module FizzBuzz.Solution
    
// HINWEIS: MOD = % in F#, Gleichheit wird mit `=` geprüft (nicht `==`)
open System
open System.Collections.Generic

let fizzBuzz n =
  if n % 15 = 0 then "FizzBuzz"
  elif n % 5 = 0 then "Buzz"
  elif n % 3 = 0 then "Fizz"
  else string n
  
let generiere n =
  seq {
    for i in [1..n] do
      yield fizzBuzz i
  }
  
let generiere2 n =
  [1..n]
  |> Seq.map fizzBuzz
  
let print() =
  let zahlen = generiere2 15
  String.Join (", ", zahlen)
  
let print_csharp() =
  let zahlen = List<string>()
  for n in [1..15] do
    zahlen.Add (fizzBuzz n)
  String.Join (", ", zahlen)