module Tests.Day7

open Xunit
open Day7

[<Fact>]
let ``correctly solves day 7 part 1`` () =
    let part1 = Puzzle.runPart1 ()
    Assert.Equal(255590, part1)
    
[<Fact>]
let ``correctly solves day 7 part 2`` () =
    let part1 = Puzzle.runPart2 ()
    Assert.Equal(58285150, part1)
