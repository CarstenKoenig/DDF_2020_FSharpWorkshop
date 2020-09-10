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

type RunState =
    | AwaitInput of (Value -> RunState)
    | QueuedOutput of Value * (unit -> RunState)
    | Halted
    
type Process =
    {
        start : unit -> RunState
    }
    
/// erstellt einen Process aus einem Program
/// beim Starten wird der Speicher immer neu initialisiert
let createProcess (program : Program) : Process =
     failwith "implement me"

/// erzeugt aus einem gegebenen Process einen neuen,
/// mit der Besonderheit, dass als erste geforderte
/// Eingabe automatisch 'input' benutzt wird
/// alle weiteren Eingaben werden über die RunState-Continuations
/// von außerhalb gefordert
let fixedInput (input : Value) (proc : Process) : Process =
    failwith "implement me"
    
/// verbindet zwei gegebene Process zu einem Neuen,
/// die Ausgaben von 'outputOf' werden dabei als Input an 'toInputOf' weitergegeben,
/// Input des Ergebnisses fließt in 'outputOf',
/// Ausgabe des Ergebnisses ist die Ausgabe von 'toInputOf'
let connect (outputOf : Process) (toInputOf: Process) : Process =
    failwith "implement me"
    
/// erzeugt einen neuen Prozess auf Basis von 'proc'
/// die (gebufferten) Ausgaben des Ergebnisses dient selbst als Eingabe
/// nur wenn keine Ausgaben vorliegen wird bei Bedarf eine weitergegeben
let feedback (proc : Process) : Process =
    failwith "implement me"