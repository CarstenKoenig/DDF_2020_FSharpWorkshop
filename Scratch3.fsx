open System

// Union
// public enum Farbe
// { Rot; Blau; }

type Farbe =
    | Rot
    | Blau of int
    | Gruen

// Maybe = option
type Maybe<'a> =
    | Just of 'a // Just = Some
    | NurEinLabel of string
    | Nothing // Nothing = None

let maybeF x =
    match x with
    | Just y -> 1
    | NurEinLabel _ -> 42
    | Nothing -> 0

let maybeF2 =
    function
    | Just y -> 1
    | NurEinLabel _ -> 42
    | Nothing -> 0

type Name_Alias = string

// Neuer Typ
type Name =
    | Name of string
    member this.Value =
        match this with
        | Name name -> name

type MyConsList<'a> =
    | Empty // []
    | Cons of 'a * MyConsList<'a> // (::)

type Expr =
    | Zahl of int
    | Plus of Expr * Expr

let rec eval (exp: Expr): int =
    match exp with
    | Zahl n -> n
    | Plus (aExpr, bExpr) -> eval aExpr + eval bExpr

// Records

type Person =
    { Name: Name
      Alter: int }
    static member Create name alter = { Name = name; Alter = alter }

let carsten = { Name = Name "Carsten"; Alter = 40 }


// Record-Update-Syntax
let carstenAelter = { carsten with Alter = 41 }


type Klasse(s: string, i: int) as thisFuerKonstruktor =
    let mutable m = string i + s
    do printf "Klasse mit %s und %d initialisiert" s i

    new() = new Klasse("X", 42)
    override this.ToString() = "Klasse"

    member this.Prop
        with get () = m
        and set x = m <- x

    member this.PropM = m

    member _.Dispose() = ()

    interface System.IDisposable with
        member this.Dispose() = this.Dispose()

type IMyInterface =
    abstract MyMethod: text:string * text2:string -> int

let myTest =
    { new IMyInterface with
        member this.MyMethod(text, text2) = 42 }

// SRTP (statically resolved type parameters)
let inline x a b = a + b
