module CoinChange.Solution

type Coin = int

let coins =
    [1; 2; 5; 10; 20; 50; 100; 200]
    |> List.rev
    
let rec wechsle rueckgabe muenzVorrat restBetrag =
    match muenzVorrat with
    | [] -> List.rev rueckgabe
    | muenze::restVorrat when muenze > restBetrag ->
        wechsle rueckgabe restVorrat restBetrag
    | muenze::_ ->
        let neuerRestBetrag = restBetrag - muenze
        wechsle (muenze::rueckgabe) muenzVorrat neuerRestBetrag
        

let change (amount : int) : Coin seq =
  wechsle [] coins amount
  |> List.toSeq

