module Program

let [<EntryPoint>] main _ =
    let day2Part1 = Day2.Puzzle.runPart1 ()
    printfn "Day2 - Part 1: %A" day2Part1
    let day2Part2 = Day2.Puzzle.runPart2 ()
    printfn "Day2 - Part 2: %A" day2Part2
    
    let day5part1 = Day5.Puzzle.runPart1 ()
    printfn "Day5 - Part 1: %A" day5part1
    let day5part2 = Day5.Puzzle.runPart2 ()
    printfn "Day5 - Part 2: %A" day5part2
    
    let day7part1 = Day7.Puzzle.runPart1 ()
    printfn "Day7 - Part 1: %A" day7part1
    let day7part2 = Day7.Puzzle.runPart2 ()
    printfn "Day7 - Part 2: %A" day7part2
    // return-code for program 
    0
