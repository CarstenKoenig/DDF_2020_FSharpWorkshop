module Cipher.Solution

open System

let private validLetters =
    [ 'A' .. 'Z' ]
    
let private validLetterSet =
    Set.ofList validLetters

type Klartext =
    private | Klartext of string
    override this.ToString () =
        let (Klartext txt) = this in txt
    static member Create (txt : string) =
        txt.ToUpper()
        |> String.filter (fun c -> Set.contains c validLetterSet)
        |> Klartext
        
type Geheimtext =
    private | Geheimtext of string
    override this.ToString () =
        let (Geheimtext txt) = this in txt
    
let verschluesseln key klartext =
    Geheimtext "implement me"
        
let entschluesseln key  geheimtext =
    Klartext "implement me"

let erstelle key =
    {|
       verschluesseln = verschluesseln key
       entschluesseln = entschluesseln key
    |}