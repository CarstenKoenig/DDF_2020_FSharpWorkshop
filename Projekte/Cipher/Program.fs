module Cipher.Program

let caesarKey = 7

let [<EntryPoint>] main _ =
    let cipher = Solution.erstelle caesarKey
   
    printf "Geheimtext? "
    let text =
        System.Console.ReadLine()
        |> Solution.Klartext.Create
   
    let geheim = cipher.verschluesseln text
    printfn "Verschlüsselt: %A" geheim
    
    let klar = cipher.entschluesseln geheim
    printfn "Entschlüsselt: %A" klar
    
    0
