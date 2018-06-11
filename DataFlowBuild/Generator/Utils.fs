// Utils.fs : Common Utilities 

module Utils

exception Error of string

open System
open System.IO

let BuildId  = Guid.NewGuid().ToString ()
let TimeStamp = DateTime.Now.ToString "F"

let Target = "Target"

let Tab = "    "

let mode = "fs"

let LogMessage (s:string) =
    printfn "%s" s

let LogWarning (s:string) =
    printfn "%s" s

let LogError (s:string) =
    printfn "%s" s


type line =
    | LineNull
    | LineFill of char * string * string //char, prefix, suffix
    | LineCenter of string * string * string * char //main, prefix, suffix, padding
    | LineStr of string * string * string //string, prefix, suffix
    | Line2Str of string * string * string * string //main1, main2, prefix, suffix

type fileItem = (string * (line list)) // FileName and Lines list

let Half x = let y = x - (x%2) in (int) (y/2)

let rec Repeat s n = if (n<0) then raise (Error("Negative argument to Repeat")) else (if (n=0) then "" else s + (Repeat s (n-1)))

let Publish (b: fileItem list) =
    let length (z:line) =
        match z with
        | LineNull -> 0
        | LineFill(_, s1, s2) -> s1.Length + s2.Length + 1
        | LineCenter(s,s1,s2,_) -> s.Length + s1.Length + s2.Length + 1
        | LineStr(s,s1,s2) -> s.Length + s1.Length + s2.Length
        | Line2Str(s1,s2,ss1,ss2) -> ss1.Length + ss2.Length + s1.Length + s2.Length + 1

    let localLength (z:line) =
        match z with
        | Line2Str(s1,s2,ss1,ss2) -> ss1.Length + s1.Length + 1
        | _ -> 0

    let max = b |> List.map snd |> List.concat |> List.map length |> List.max

    let print (m:int) (z:line) : string =
        match z with
        | LineNull -> ""
        | LineFill(c, s1, s2) -> let p = (max - s1.Length - s2.Length) in let s = Repeat (new string [|c|]) p in s1 + s + s2
        | LineCenter(s,s1,s2,c) ->
            let p = max - s1.Length - s2.Length - s.Length in let q1 = Half p in let q2 = p - q1
            s1 + (Repeat (new string [|c|]) q1) + s + (Repeat (new string [|c|]) q2) + s2
        | LineStr(s,s1,s2) -> s1 + s + (Repeat " " (max - s.Length - s1.Length)) + s2
        | Line2Str(s1,s2,ss1,ss2) -> 
            let st1 = Repeat " " (m - ss1.Length - s1.Length)
            ss1 + s1 + st1 + s2 + (Repeat " " (max - st1.Length - s2.Length - m + 1)) + ss2
    
    for (f,l) in b do
        let m = l |> List.map localLength |> List.max
        File.WriteAllLines (Path.Combine (Target,f), (List.map (print m) l))
    max
