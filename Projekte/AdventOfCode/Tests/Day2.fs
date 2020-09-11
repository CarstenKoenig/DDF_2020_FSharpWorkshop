module Tests.Day2

open Xunit
open Day2

let runInput input =
    Intcode.parse input
    |> Intcode.initMemory
    |> Intcode.eval
    |> string

[<Fact>]
let ``evals 1+1 to 2`` () =
    Assert.Equal("2,0,0,0,99", runInput "1,0,0,0,99")
    
[<Fact>]
let ``evals 2*3 to 6`` () =
    Assert.Equal("2,3,0,6,99", runInput "2,3,0,3,99")
    
[<Fact>]
let ``evals 99*99 to 9801`` () =
    Assert.Equal("2,4,4,5,99,9801", runInput "2,4,4,5,99,0")
    
[<Fact>]
let ``evals complex program rewriting a Halt to multiply 5*6 with result 30`` () =
    Assert.Equal("30,1,1,4,2,5,6,0,99", runInput "1,1,1,4,99,5,6,0,99")
   
[<Fact>]
let ``correctly solves day 2 part 1`` () =
    let part1 = Puzzle.runPart1 ()
    Assert.Equal(4570637, part1)
    
[<Fact>]
let ``correctly solves day 2 part 2`` () =
    let part2 = Puzzle.runPart2 ()
    Assert.Equal(5485, part2)
  
