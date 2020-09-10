module Tests.Day5

open Xunit
open Day5

[<Fact>]
let ``correctly solves day 5 part 1`` () =
    let part1 = Puzzle.runPart1 ()
    Assert.Equal(15259545, part1)
    
[<Fact>]
let ``correctly solves day 5 part 2`` () =
    let part1 = Puzzle.runPart2 ()
    Assert.Equal(7616021, part1)
