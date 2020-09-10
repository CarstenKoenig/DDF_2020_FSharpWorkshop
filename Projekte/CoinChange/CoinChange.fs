module CoinChange.Solution

type Coin = int

let coins =
    [1; 2; 5; 10; 20; 50; 100; 200]

let rec private recursiveChange remCoins remAmount =
  if remAmount = 0 then [] else
  match remCoins with
    | [] ->
      failwith "ich kann darauf nicht zurückgeben"
    | (c::_) when c <= remAmount ->
      c :: recursiveChange remCoins (remAmount - c)
    | (_::cs) ->
      recursiveChange cs remAmount
      
let private unfoldChange coins amount =
  (coins, amount)
  |> Seq.unfold
    (function
      | _, 0 -> None
      | coin::remainingCoins, remainingAmount ->
          let count = remainingAmount / coin
          let restAmount = remainingAmount - count * coin
          Some (Seq.replicate count coin, (remainingCoins, restAmount))
       | _ -> failwith "das sollte nie passieren"
    )
  |> Seq.concat

let change (amount : int) : Coin seq =
  let coins = List.sort coins |> List.rev
  unfoldChange coins amount

