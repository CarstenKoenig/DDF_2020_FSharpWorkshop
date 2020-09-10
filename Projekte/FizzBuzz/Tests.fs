module FizzBuzz.Tests

open Hedgehog
open Xunit
open FizzBuzz

module ``Teste die fizzBuzz Funktion`` =

    [<Fact>]
    let ``alle Vielfachen von 3 beginnen mit Fizz`` () =
        property {
            let! n = Gen.int (Range.constant 1 100)
            let result = Solution.fizzBuzz (3*n)
            counterexample (sprintf "Ergebnis zu n=%d war %s" n result)
            return
                result.StartsWith "Fizz"
        }
        |> Property.check
        
    [<Fact>]
    let ``alle Vielfachen von 5 enden auf Buzz`` () =
        property {
            let! n = Gen.int (Range.constant 1 100)
            let result = Solution.fizzBuzz (5*n)
            counterexample (sprintf "Ergebnis zu n=%d war %s" n result)
            return result.EndsWith "Buzz"
        }
        |> Property.check
        
    [<Fact>]
    let ``alle Vielfachen von 15 sind FizzBuzz`` () =
        property {
            let! n = Gen.int (Range.constant 1 100)
            let result = Solution.fizzBuzz (15*n)
            counterexample (sprintf "Ergebnis zu n=%d war %s" n result)
            return result = "FizzBuzz"
        }
        |> Property.check
        
    [<Fact>]
    let ``alle Zahlen n die weder von 3 noch von 5 teilbar sind werden einfach als String zurückgegeben`` () =
        property {
            let! n = Gen.int (Range.constant 1 100)
            where (n % 3 <> 0 && n % 5 <> 0)
            let result = Solution.fizzBuzz n
            counterexample (sprintf "Ergebnis zu n=%d war %s" n result)
            return result = string n
        }
        |> Property.check

module ``Teste 'print' Funktion`` =
    [<Fact>]
    let ``gibt den korrekten String von 1 bis 15 zurück`` () =
        Assert.Equal("1, 2, Fizz, 4, Buzz, Fizz, 7, 8, Fizz, Buzz, 11, Fizz, 13, 14, FizzBuzz", Solution.print())