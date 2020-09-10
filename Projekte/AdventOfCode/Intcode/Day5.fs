module Day5.Intcode

open System

type Value = int
type Program = unit
type Memory = unit

type RunState =
    | AwaitInput of (Value -> RunState)
    | QueuedOutput of Value * (unit -> RunState)
    | Halted

let parse (input : string) : Program =
    failwith "implement me"
let initMemory (program : Program) : Memory =
    failwith "implement me"
let start (memory : Memory) : RunState =
    failwith "implement me"
