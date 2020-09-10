module Day5.Intcode

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

//----------------------------------------------------------------------
// Änderung für Tag 5 - Verschiedene Addressierungsarten (Mode) als Parameter
// für 'OpCode'

/// Addressiert werden kann über Position (wie bisher)
/// oder 'Immediate' - ist eigentlich nur ein Konstanter Wert
type Parameter =
    | PositionParameter of address:Address
    | ImmediateParameter of value:Value

/// je nach Addressierungsart den Speicher auslesen oder den Konstanten Wert zurückgeben
let readAtParam (p : Parameter) (memory : Memory) =
    match p with
    | PositionParameter adr -> readAt adr memory
    | ImmediateParameter value -> value
    

/// schreiben macht nur über Position Sinn - Immediate führt zur Exception
let writeToParam (p : Parameter) (value : Value) =
    match p with
    | PositionParameter adr -> writeTo adr value
    | ImmediateParameter _ -> failwith "cannot write to an immediate value - sorry"
   
 
//----------------------------------------------------------------------
// Änderung für Tag 5 - Verschiedene Addressierungsarten (Mode) als Parameter
// für 'OpCode'
// Part 1 bekommt zusützliche OpCodes für Input/Output
// Part 2 erweitert das mit Jumps und Vergleichen
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
              // der OpCode ist in den niedrigesten 2 Stellen
              // Rest bei Division durch 100 liefert diesen
              value % 100
          getParam =
              fun n ->
                  let memoryValue = readAt (adr+n) memory
                  // ab der 1000-er Stelle beginnt der Mode für den 1.ten Parameter!
                  // d.h. 2-Stellen (10^2) müssen entfernt werden - Ganzahldivision macht das!
                  // da dort nur eine Dezimalstelle interessiert noch `mod 10`
                  // allgemein für den n-ten Parameter: n+1 Stellen müssen weg - die nächste Dezimalziffer interessiert
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

//----------------------------------------------------------------------
// Änderung für Tag 5
// da Input/Output dazukommt können wir nicht mehr einfach bis zum Halt laufen
// lassen
// Es kann sein das über einen (Problem-spezifischen) Weg Input/Output zu erfolgen hat
// wir lösen das, indem wir 'StepState' einführen

/// spiegelt den aktuellen Zustand beim Schrittweise Ausführen eines Programms dar
/// zusätzlich zu 'Halted' und 'Continue' (was den bisherigen Step entspricht)
/// bekommen wir zwei weitere Zustände:
/// - 'AwaitInput' - eine *Continuation* - wenn wird dieser Funktion einen Wert geben
/// führt sie den Step der auf Input wartet zu Ende
/// - 'QueuedOutput' die Ausgabe zusammen mit dem Ergebnis des Steps
type StepState =
    | AwaitInput of cont:(Value -> (InstructionPointer * Memory))
    | QueuedOutput of output:Value * cont:(InstructionPointer * Memory)
    | Continue of cont:(InstructionPointer * Memory)
    | Halted

/// wie bisher allerdings übernimmt diese Funktion jetzt die verwaltung
/// des InstructionPointers mit (wegen der Sprünge im Teil 2)
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
        
/// führt wie bisher einen Einzelschritt durch, wobei jetzt aber ein 'StepState'
/// geliefert wird
let step (ip : InstructionPointer, memory : Memory) =
    let opCode = getOpCodeAt ip memory
    executeOpcode ip opCode memory

/// eine Variante von 'StepState' nur wird hier 'step' durchgeführt
/// bis Halt/Input/Output erfolgt
type RunState =
    | AwaitInput of (Value -> RunState)
    | QueuedOutput of Value * (unit -> RunState)
    | Halted
    
/// führt solange steps durch bis Halt/Input/Output erfolgt
let rec run state =
    match step state with
    | StepState.Halted ->
        RunState.Halted
    | StepState.Continue nextState ->
        run nextState
    | StepState.AwaitInput cont ->
        RunState.AwaitInput (fun value -> run (cont value))
    | StepState.QueuedOutput (output, cont) ->
        RunState.QueuedOutput (output, fun () -> run cont)
       

/// startet die Verabeitung mit IP 0 im übergebenen Speicher bis zum ersten 'RunState' ergebnis
let start memory =
    run (0, memory)
