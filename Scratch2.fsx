open System

let tuple = 1, "Hallo"
let tuple2 = 'c', true, "Hallo"

//  (1   , "Hallo")
let zahl, text = tuple
let _, bool1, _ = tuple2

let f x =
    match x with
    | (_, false, _) -> "false"
    | (_, true, y : string)
        when y.Length = 3 -> "3"
    | (_, true, y) -> y
    
// Listen

let liste = [1; 2; 3]
let listRange = [1..3..15]
let liste_2 = 1::(2::(3::[]))

let myHead ls =
    match ls with
    | h::_ -> h
    | [] -> failwith "empty list"

let myTail ls =
    match ls with
    | _::tl -> tl
    | [] -> failwith "empty list" 
    
    
let rec append xs ys =
    match xs with
    | [] -> ys
    | h::tl -> h :: append tl ys
