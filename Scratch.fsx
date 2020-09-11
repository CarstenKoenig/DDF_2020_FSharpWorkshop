open System

let zahl = 5
let text = "Hallo"
let dezimal = 2.25m
let flies: double = 2.24
let zeit = DateTime.Now

type Zahl = int

// using Zahl = System.Int32

type Mit<'a> = 'a
type 'a Mit2 = 'a
// int list = list<int>
// int option = option<int>

let unit = () // Void

let bottom: int = failwith "??"


/// Funktionen
///

let funktion_name argument = argument + 5


// "currying"
// int -> (int -> int)
let plus x y = x + y

let plus10 x = plus 10 x

// Pipe Operator |>
let plus10_2 x = x |> plus 10 |> string

let grossSchreiben (x: string): string = x.ToUpper()

let falle = grossSchreiben ("welt".Trim())

// fact n = 1 * 2 * ... * n
let rec fact n = if n <= 1 then 1 else n * fact (n - 1)
