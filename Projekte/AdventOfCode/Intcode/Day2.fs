module Day2.Intcode

/// Ein Wert/Zahl im Programm oder Speicher
type Value =
    int
    
/// Ein Programm ist eine Auflistung von Values
type Program =
    Program of Value seq

/// liest einen String von, durch ',' getrennte Values in ein Programm
let parse (input : string) : Program =
    input
    |> fun s -> s.Split [| ',' |]
    |> Seq.map System.Int32.Parse
    |> Program

/// Addresse/Index für den Speicher (0-basierend)
type Address =
    int
    
/// addressierbarer Speicher
type Memory =
    | Memory of Map<Address, Value>
    /// liefert gleiches Format wie aus der Puzzlebeschreibung
    /// ("Value,Value,Value,...,Value")
    override this.ToString() =
        let (Memory map) = this
        map
        |> Seq.map (fun kvp -> string kvp.Value)
        |> fun values -> System.String.Join (",", values)
    
/// initialisiert Speicher aus einem Program
let initMemory (Program from) : Memory =
    from
    |> Seq.mapi (fun adr value -> (adr, value))
    |> Map.ofSeq
    |> Memory

/// liest den Inhalt des Speichers an der gegebenen Addresse
/// wirft eine Exception falls die Addresse ungültig ist
let readAt adr (Memory memory) =
    memory.[adr]
    
/// liefert eine Speicher-Kopie die an 'adr' den Wert 'value' enthält
let writeTo adr value (Memory memory) =
    Memory (memory.Add (adr, value))
 
/// unterstützes Befehlsset
type OpCode =
    | OpAdd of addressFirstOperand:Address * addressSecondOperand:Address * addressOutput:Address
    | OpMul of addressFirstOperand:Address * addressSecondOperand:Address * addressOutput:Address
    | OpHalt

/// liest einen Opcode ab 'adr' aus dem Speicher
/// wirft Exceptions wenn das nicht möglich ist
let getOpCodeAt (adr : Address) (memory : Memory) =
    match readAt adr memory with
    | 1 ->
        let adrA = readAt (adr+1) memory
        let adrB = readAt (adr+2) memory
        let adrOut = readAt (adr+3) memory
        OpAdd (adrA, adrB, adrOut)
    | 2 ->
        let adrA = readAt (adr+1) memory
        let adrB = readAt (adr+2) memory
        let adrOut = readAt (adr+3) memory
        OpMul (adrA, adrB, adrOut)
    | 99 ->
        OpHalt
    | unknown ->
        failwithf "unknown OpCode %d at address %d" unknown adr

/// wieviel Speicher-Zellen besetzt der übergebene 'opcode'?
let opCodeLen (opcode : OpCode) =
    match opcode with
    | OpHalt -> 1
    | OpAdd _ -> 4
    | OpMul _ -> 4
        
/// verarbeitet 'opcode'
/// bei 'OpHalt' wird einfach 'None' geliefert sonst
/// wird eine Kopie von 'memory' geliefert die durch das Ausführen von 'opcode' entsteht
let executeOpcode (opcode : OpCode) (memory : Memory) =
    match opcode with
    | OpHalt ->
        None
    | OpAdd (adrA, adrB, outAdr) ->
        let valA = readAt adrA memory
        let valB = readAt adrB memory
        Some (writeTo outAdr (valA + valB) memory)
    | OpMul (adrA, adrB, outAdr) ->
        let valA = readAt adrA memory
        let valB = readAt adrB memory
        Some (writeTo outAdr (valA * valB) memory)
        
/// an welcher Stelle im Speicher soll der nächste Opcode/Befehl gelesen werden
type InstructionPointer =
    Address

/// ein Verarbeitungsschritt:
/// liest den OpCode an Addresse 'ip' und wendet
/// diesen auf 'memory' an - liefert 'None' falls
/// es ein 'Halt' war oder 'Some (neueIp, neuerMemory)'
/// fertig für den nächsten Step
let step (ip : InstructionPointer, memory : Memory) =
    let opCode = getOpCodeAt ip memory
    executeOpcode opCode memory
    |> Option.map (fun updatedMemory -> (ip + opCodeLen opCode, updatedMemory))

/// führt 'step' bis zum halt durch
let run (memory : Memory) =
    Seq.unfold
        (fun state ->
            step state
            // unfold into (IP / Memory) pairs
            |> Option.map (fun res -> (res,res))
        )
        (0, memory)
        
/// führt 'step' bis zum halt durch und liefert
/// den letzten Speicher-Zustand davor
let eval (memory : Memory) =
    run memory
    |> Seq.last
    |> snd