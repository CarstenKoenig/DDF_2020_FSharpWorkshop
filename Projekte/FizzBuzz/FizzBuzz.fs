module FizzBuzz.Solution
    
  // HINWEIS: MOD = % in F#, Gleichheit wird mit `=` geprüft (nicht `==`)

  let fizzBuzz n =
    match n with
      | _ when n % 15 = 0 -> "FizzBuzz"
      | _ when n % 5  = 0 -> "Buzz"
      | _ when n % 3  = 0 -> "Fizz"
      | _                 -> string n

  let generate maxN =
    [ 1..maxN ]
    |> Seq.map fizzBuzz
    
  let print() =
    generate 15
    |> fun items -> System.String.Join (", ", items)