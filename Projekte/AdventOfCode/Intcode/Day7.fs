module Day7.Intcode

open System

type Value =
    int
    
type Program =
    Program of Value seq

let parse (input : string) : Program =
    input
    |> fun s -> s.Split [| ',' |]
    |> Seq.map Int32.Parse
    |> Program

type Address =
    int
    
type Memory =
    | Memory of Value array
    override this.ToString() =
        let (Memory map) = this
        map
        |> fun values -> String.Join (",", values)
    
let initMemory (Program from) : Memory =
    from
    |> Seq.toArray
    |> Memory

let readAt adr (Memory memory) =
    memory.[adr]
    
let writeTo adr value (Memory memory) =
    let memory' = Array.copy memory
    memory'.[adr] <- value
    Memory memory'
    
type Parameter =
    | PositionParameter of address:Address
    | ImmediateParameter of value:Value

let readAtParam (p : Parameter) (memory : Memory) =
    match p with
    | PositionParameter adr -> readAt adr memory
    | ImmediateParameter value -> value
    
let writeToParam (p : Parameter) (value : Value) =
    match p with
    | PositionParameter adr -> writeTo adr value
    | ImmediateParameter _ -> failwith "cannot write to an immediate value - sorry"
    
type OpCode =
    | OpAdd of firstOperand:Parameter * secondOperand:Parameter * output:Parameter
    | OpMul of firstOperand:Parameter * secondOperand:Parameter * output:Parameter
    | OpInput of parameter:Parameter
    | OpOutput of parameter:Parameter
    | OpJumpIfTrue of test:Parameter*jumpTo:Parameter
    | OpJumpIfFalse of test:Parameter*jumpTo:Parameter
    | OpLessThan of val1:Parameter*val2:Parameter*out:Parameter
    | OpEquals of val1:Parameter*val2:Parameter*out:Parameter
    | OpHalt
    
let opCodeLen (opcode : OpCode) =
    match opcode with
    | OpHalt -> 1
    | OpAdd _ -> 4
    | OpMul _ -> 4
    | OpInput _ -> 2
    | OpOutput _ -> 2
    | OpJumpIfFalse _ -> 3
    | OpJumpIfTrue _ -> 3
    | OpLessThan _ -> 4
    | OpEquals _ -> 4
        
let getOpCodeAt (adr : Address) (memory : Memory) =
    let parseParameter value =
        {|
          opcode =
              value % 100
          getParam =
              fun n ->
                  let memoryValue = readAt (adr+n) memory
                  match (value / pown 10 (n+1)) % 10 with
                  | 0 -> PositionParameter memoryValue
                  | 1 -> ImmediateParameter memoryValue
                  | unknown -> failwithf "unknown position-mode for parameter %d in opcode %d: %d" n value unknown
        |}
    let parameter = parseParameter (readAt adr memory)
    match parameter with
    | p when p.opcode = 1 ->
        let param1 = p.getParam 1
        let param2 = p.getParam 2
        let param3 = p.getParam 3
        OpAdd (param1, param2, param3)
    | p when p.opcode = 2 ->
        let param1 = p.getParam 1
        let param2 = p.getParam 2
        let param3 = p.getParam 3
        OpMul (param1, param2, param3)
    | p when p.opcode = 3 ->
        let param1 = p.getParam 1
        OpInput param1
    | p when p.opcode = 4 ->
        let param1 = p.getParam 1
        OpOutput param1
    | p when p.opcode = 5 ->
        let param1 = p.getParam 1
        let param2 = p.getParam 2
        OpJumpIfTrue (param1, param2)
    | p when p.opcode = 6 ->
        let param1 = p.getParam 1
        let param2 = p.getParam 2
        OpJumpIfFalse (param1, param2)
    | p when p.opcode = 7 ->
        let param1 = p.getParam 1
        let param2 = p.getParam 2
        let param3 = p.getParam 3
        OpLessThan (param1, param2, param3)
    | p when p.opcode = 8 ->
        let param1 = p.getParam 1
        let param2 = p.getParam 2
        let param3 = p.getParam 3
        OpEquals (param1, param2, param3)
    | p when p.opcode = 99 ->
        OpHalt
    | unknown -> failwithf "unknown OpCode %d at address %d" unknown.opcode adr

type InstructionPointer =
    Address

type StepState =
    | AwaitInput of cont:(Value -> (InstructionPointer * Memory))
    | QueuedOutput of output:Value * cont:(InstructionPointer * Memory)
    | Continue of cont:(InstructionPointer * Memory)
    | Halted

let executeOpcode (ip : InstructionPointer) (opcode : OpCode) (memory : Memory) =
    let nextIp = ip + opCodeLen opcode
    match opcode with
    | OpHalt -> Halted
    | OpAdd (paramA, paramB, paramOut) ->
        let valA = readAtParam paramA memory
        let valB = readAtParam paramB memory
        Continue (nextIp, writeToParam paramOut (valA + valB) memory)
    | OpMul (paramA, paramB, paramOut) ->
        let valA = readAtParam paramA memory
        let valB = readAtParam paramB memory
        Continue (nextIp, writeToParam paramOut (valA * valB) memory)
    | OpInput param ->
        AwaitInput (fun value -> (nextIp, writeToParam param value memory))
    | OpOutput param ->
        let value = readAtParam param memory
        QueuedOutput (value, (nextIp, memory))
    | OpLessThan (paramA, paramB, paramOut) ->
        let valA = readAtParam paramA memory
        let valB = readAtParam paramB memory
        Continue (nextIp, writeToParam paramOut (if valA < valB then 1 else 0) memory)
    | OpEquals (paramA, paramB, paramOut) ->
        let valA = readAtParam paramA memory
        let valB = readAtParam paramB memory
        Continue (nextIp, writeToParam paramOut (if valA = valB then 1 else 0) memory)
    | OpJumpIfTrue (paramA, paramB) ->
        let valA = readAtParam paramA memory
        let valB = readAtParam paramB memory
        let gotoIp = if valA <> 0 then valB else nextIp
        Continue (gotoIp, memory)
    | OpJumpIfFalse (paramA, paramB) ->
        let valA = readAtParam paramA memory
        let valB = readAtParam paramB memory
        let gotoIp = if valA = 0 then valB else nextIp
        Continue (gotoIp, memory)
        
let step (ip : InstructionPointer, memory : Memory) =
    let opCode = getOpCodeAt ip memory
    executeOpcode ip opCode memory

type RunState =
    | AwaitInput of (Value -> RunState)
    | QueuedOutput of Value * (unit -> RunState)
    | Halted
    
let rec private run state =
    match step state with
    | StepState.Halted ->
        RunState.Halted
    | StepState.Continue nextState ->
        run nextState
    | StepState.AwaitInput cont ->
        RunState.AwaitInput (fun value -> run (cont value))
    | StepState.QueuedOutput (output, cont) ->
        RunState.QueuedOutput (output, fun () -> run cont)
       
let start memory =
    run (0, memory)

// ----------------------------------------------------------------------
// Start Tag 7

/// ein Process stellt ist eine Abstraktion einer "Black Box", die nach dem
/// starten ein Intcode-Programm ablaufen lässt (wie bishe über Run-States) 
type Process =
    {
        start : unit -> RunState
    }
    
/// erstellt einen Process aus einem Program
/// beim Starten wird der Speicher immer neu initialisiert
let createProcess (program : Program) =
     {
         start = fun () -> start (initMemory program)
     }
    
/// erzeugt aus einem gegebenen Process einen neuen,
/// mit der Besonderheit, dass als erste geforderte
/// Eingabe automatisch 'input' benutzt wird
/// alle weiteren Eingaben werden über die RunState-Continuations
/// von außerhalb gefordert
let fixedInput (input : Value) (proc : Process) =
    let rec provideInput =
        function
        | RunState.Halted ->
            RunState.Halted 
        | RunState.AwaitInput cont ->
            cont input
        | RunState.QueuedOutput (output, cont) ->
            RunState.QueuedOutput (output, fun () -> provideInput (cont ()))
    {
        start = fun () -> provideInput (proc.start())
    }
    
/// verbindet zwei gegebene Process zu einem Neuen,
/// die Ausgaben von 'outputOf' werden dabei als Input an 'toInputOf' weitergegeben,
/// Input des Ergebnisses fließt in 'outputOf',
/// Ausgabe des Ergebnisses ist die Ausgabe von 'toInputOf'
let connect (outputOf : Process) (toInputOf: Process) =
    let rec runCon source sink =
        match source, sink with
        | RunState.Halted, RunState.Halted ->
            RunState.Halted
        | _, RunState.Halted ->
            failwithf "sink process halted with source still active - deadlock"
        | RunState.Halted, RunState.AwaitInput _ ->
            failwithf "process died - sink is waiting for input but source halted"
        | RunState.QueuedOutput (out, contSource), AwaitInput contSink ->
            runCon (contSource ()) (contSink out)
        | _, RunState.QueuedOutput (output, cont) ->
            RunState.QueuedOutput (output, fun () -> runCon source (cont ()))
        | RunState.AwaitInput cont, _ ->
            RunState.AwaitInput (fun value -> runCon (cont value) sink)
    {
        start = fun () -> runCon (outputOf.start()) (toInputOf.start())
    }

/// erzeugt einen neuen Prozess auf Basis von 'proc'
/// die (gebufferten) Ausgaben des Ergebnisses dient selbst als Eingabe
/// nur wenn keine Ausgaben vorliegen wird bei Bedarf eine weitergegeben
let feedback (proc : Process) =
    let rec runFeedback buffer state =
        match state, buffer with
        | RunState.Halted, _ ->
            RunState.Halted
        | RunState.AwaitInput cont, (firstOutput::restBuffer) ->
            runFeedback restBuffer (cont firstOutput)
        | RunState.AwaitInput cont, [] ->
            RunState.AwaitInput (fun value -> runFeedback [] (cont value))
        | RunState.QueuedOutput (output, cont), buffer ->
            RunState.QueuedOutput (output, fun () -> runFeedback (buffer @ [output]) (cont ()))
    {
        start = fun () -> runFeedback [] (proc.start ())
    }