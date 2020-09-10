module CoinChange.Tests

open Hedgehog
open Xunit

[<Fact>]
let ``die zurückgegebene Summe sollte den übergebenen Betrag entsprechen`` () =
    property {
        let! betragCent = Gen.int (Range.constant 1 500)
        let muenzen = Solution.change betragCent
        counterexample (sprintf "auf %dct wurde %A gewechselt" betragCent muenzen)
        return Seq.sum muenzen = betragCent
    }
    |> Property.check
    
[<Fact>]
let ``wechselt 143 in [1€, 20ct, 20ct, 2ct, 1ct]`` () =
    Assert.Equal<int>([100; 20; 20; 2; 1], Solution.change 143)
