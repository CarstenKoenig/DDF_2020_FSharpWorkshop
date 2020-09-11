module Day2.Intcode

/// Ein Wert/Zahl im Programm oder Speicher
type Value =
    int
    
/// Ein Programm ist eine Auflistung von Values
type Program =
    Program of Value seq

/// liest einen String von, durch ',' getrennte Values in ein Programm
let parse (input : string) : Program =
    input.Split [| ',' |]
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
        match this with
        | Memory m ->
            m
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.map string
            |> fun s -> System.String.Join(",", s)
    
/// initialisiert Speicher aus einem Program
// let initMemory (Program value) : Memory =
let initMemory program : Memory =
    match program with
    | Program values ->
        values
        |> Seq.mapi (fun index value -> (index, value))
        |> Map.ofSeq
        |> Memory

/// liest den Inhalt des Speichers an der gegebenen Addresse
/// wirft eine Exception falls die Addresse ungültig ist
let readAt adr memory =
    match memory with
    | Memory m -> m.[adr]
    
/// liefert eine Speicher-Kopie die an 'adr' den Wert 'value' enthält
let writeTo adr value memory =
    match memory with
    | Memory m ->
        m
        |> Map.add adr value
        |> Memory
 
/// unterstützes Befehlsset
type OpCode =
    | OpAdd of addressFirstOperand:Address * addressSecondOperand:Address * addressOutput:Address
    | OpMul of addressFirstOperand:Address * addressSecondOperand:Address * addressOutput:Address
    | OpHalt

/// liest einen Opcode ab 'adr' aus dem Speicher
/// wirft Exceptions wenn das nicht möglich ist
let getOpCodeAt (adr : Address) (memory : Memory) : OpCode =
    match readAt adr memory with
    | 1 ->
        // ADD
        let op1Adr = readAt (adr+1) memory
        let op2Adr = readAt (adr+2) memory
        let outAdr = readAt (adr+3) memory
        OpAdd (op1Adr, op2Adr, outAdr)
    | 2 ->
        // Mult
        let op1Adr = readAt (adr+1) memory
        let op2Adr = readAt (adr+2) memory
        let outAdr = readAt (adr+3) memory
        OpMul (op1Adr, op2Adr, outAdr)
    | 99 ->
        OpHalt
    | unbekannt ->
        failwithf "OpCode %d an Adressen %d unbekannt" unbekannt adr

/// wieviel Speicher-Zellen besetzt der übergebene 'opcode'?
let opCodeLen (opcode : OpCode) : int =
    match opcode with
    | OpAdd _ -> 4
    | OpMul _ -> 4
    | OpHalt -> 1
        
/// verarbeitet 'opcode'
/// bei 'OpHalt' wird einfach 'None' geliefert sonst
/// wird eine Kopie von 'memory' geliefert die durch das Ausführen von 'opcode' entsteht
let executeOpcode (opcode : OpCode) (memory : Memory) : Memory option =
    match opcode with
    | OpHalt -> None
    | OpAdd (adr1, adr2, adrOut) ->
        let val1 = readAt adr1 memory
        let val2 = readAt adr2 memory
        Some (writeTo adrOut (val1+val2) memory)
    | OpMul (adr1, adr2, adrOut) ->
        let val1 = readAt adr1 memory
        let val2 = readAt adr2 memory
        Some (writeTo adrOut (val1*val2) memory)
        
/// an welcher Stelle im Speicher soll der nächste Opcode/Befehl gelesen werden
type InstructionPointer =
    Address

/// ein Verarbeitungsschritt:
/// liest den OpCode an Addresse 'ip' und wendet
/// diesen auf 'memory' an - liefert 'None' falls
/// es ein 'Halt' war oder 'Some (neueIp, neuerMemory)'
/// fertig für den nächsten Step
let step (ip : InstructionPointer, memory : Memory)
    : (InstructionPointer * Memory) option =
    let op = getOpCodeAt ip memory
    match executeOpcode op memory with
    | None -> None
    | Some newMemory ->
        Some (ip + opCodeLen op, newMemory)

/// führt 'step' bis zum halt durch und liefert
/// den letzten Speicher-Zustand davor
let eval (memory : Memory) : Memory =
    let rec go (ip, curMemory) =
        match step (ip, curMemory) with
        | None -> curMemory
        | Some state -> go state
    go (0, memory)
