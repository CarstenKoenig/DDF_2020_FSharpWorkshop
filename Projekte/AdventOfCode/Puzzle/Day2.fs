module Day2.Puzzle

open Day2
    
let myProgram =
    "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,10,23,27,2,27,13,31,1,10,31,35,1,35,9,39,2,39,13,43,1,43,5,47,1,47,6,51,2,6,51,55,1,5,55,59,2,9,59,63,2,6,63,67,1,13,67,71,1,9,71,75,2,13,75,79,1,79,10,83,2,83,9,87,1,5,87,91,2,91,6,95,2,13,95,99,1,99,5,103,1,103,2,107,1,107,10,0,99,2,0,14,0"
    |> Intcode.parse
    
let run noun verb =
    myProgram
    |> Intcode.initMemory
    |> Intcode.writeTo 1 noun
    |> Intcode.writeTo 2 verb
    |> Intcode.eval
    |> Intcode.readAt 0
    
let runPart1 () =
    run 12 2
    
let runPart2 () =
    let goal = 19690720
    let foundPairs =
        seq {
            for noun in [ 0..99 ] do
            for verb in [ 0..99 ] do
            let result = run noun verb
            if result = goal then yield (noun, verb)
        }
    foundPairs
    |> Seq.head
    |> fun (noun,verb) -> 100 * noun + verb
