module Day7.Puzzle

open Day7
    
let myProgram =
    "3,8,1001,8,10,8,105,1,0,0,21,42,63,76,101,114,195,276,357,438,99999,3,9,101,2,9,9,102,5,9,9,1001,9,3,9,1002,9,5,9,4,9,99,3,9,101,4,9,9,102,5,9,9,1001,9,5,9,102,2,9,9,4,9,99,3,9,1001,9,3,9,1002,9,5,9,4,9,99,3,9,1002,9,2,9,101,5,9,9,102,3,9,9,101,2,9,9,1002,9,3,9,4,9,99,3,9,101,3,9,9,102,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99"
    |> Intcode.parse
    
let connectInSeries phases =
    phases
    |> List.map
           (fun phase ->
                Intcode.createProcess myProgram
                |> Intcode.fixedInput phase
           )
    |> List.reduce Intcode.connect
    |> Intcode.fixedInput 0
        
let runPart1 () =
    let runInSeriesWith phases =
        let runToOutput state =
            match state with
            | Intcode.RunState.Halted -> failwith "process halted without output"
            | Intcode.RunState.AwaitInput _ -> failwith "process wants input - don't know what to do"
            | Intcode.RunState.QueuedOutput (output, _) -> output
       
        connectInSeries phases
        |> fun p -> p.start ()
        |> runToOutput
    Combinatorics.permute [0..4]
    |> List.map runInSeriesWith
    |> List.max
    
let runWithFeedback phases =
    let rec getOutputSeq state =
        seq {
            match state with
            | Intcode.RunState.Halted -> ()
            | Intcode.RunState.AwaitInput _ -> failwith "process wants input - don't know what to do"
            | Intcode.RunState.QueuedOutput (output, cont) ->
                yield output
                yield! getOutputSeq (cont ())
        }
    connectInSeries phases
    |> Intcode.feedback
    |> fun p -> p.start ()
    |> getOutputSeq
    |> Seq.last
    
let runPart2 () =
    Combinatorics.permute [5..9]
    |> List.map runWithFeedback
    |> List.max
