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

let rec private mapString key =
    if key < 0 then mapString (validLetters.Length + key) else
    if key > validLetters.Length then mapString (key % validLetters.Length) else
    let cycledLetters = List.skip key validLetters @ List.take key validLetters
    let map =
        List.zip validLetters cycledLetters
        |> Map.ofList
    fun txt ->
        txt
        |> Seq.choose map.TryFind
        |> Seq.toArray
        |> String
    
let verschluesseln key =
    let f = mapString key
    fun (Klartext txt) ->
        f txt
        |> Geheimtext
        
let entschluesseln key =
    let f = mapString (-key)
    fun (Geheimtext txt) ->
        f txt
        |> Klartext

let erstelle key =
    {|
       verschluesseln = verschluesseln key
       entschluesseln = entschluesseln key
    |}