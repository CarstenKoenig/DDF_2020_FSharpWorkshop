module Cipher.Tests

open Hedgehog
open Xunit

let private genKlartext lenRange : Solution.Klartext Gen  =
    let letter = Gen.char 'A' 'Z'
    Gen.string lenRange letter
    |> Gen.map Solution.Klartext.Create

[<Fact>]
let ``Entschluesseln ist links-invers zu Verschluesseln`` () =
    property {
        let! key = Gen.int (Range.constant -100 100)
        let cipher = Solution.erstelle key
        let! klar = genKlartext (Range.linear 1 10)
        let geheim = cipher.verschluesseln klar
        let entschluesselt = cipher.entschluesseln geheim
        counterexample (sprintf "Entschlüsselung von %A war %A sollte aber %A sein" geheim entschluesselt klar)
        return klar = entschluesselt
    }
    |> Property.check
    
[<Fact>]
let ``Verschluesseln von HALLO mit Key 7 ist OHSSV``() =
    Assert.Equal ("OHSSV", string (Solution.Klartext.Create "HALLO" |> Solution.verschluesseln 7) )

[<Fact>]
let ``Entschluesseln von OHSSV mit Key 7 ist HALLO``() =
    let geheim = Solution.Klartext.Create "HALLO" |> Solution.verschluesseln 7
    Assert.Equal ("HALLO", string (Solution.entschluesseln 7 geheim))
