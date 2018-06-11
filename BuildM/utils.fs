(* *****************************************************************
      UTILS.FS: Common Utilities to be inherited by every modules
   ***************************************************************** *)

module Utils

open Definations
open System.IO

type maybe<'T> = Yes of 'T | No | Err of string

(* Filteroptions: Remove all the options and collect the ekements *)
let FilterOptions Z = List.fold (fun S Y -> match Y with Some(X) -> X::S | None -> S) [] Z |> List.rev

(* Print Error Message with Types *)
let private PrintError t ((f, l, n) : Position) s = printfn "%s Error %s (%i:%i) : %s" t f l n s

(* Error Handling Utilties *)
type Error() =
    static let mutable count : int = 0
    static member System s =  count <- count + 1 ; printfn "System Error: %s" s
    static member File (f : string) s =  count <- count + 1 ; printfn "File Error %s : %s" f s
    static member Syntax p s = count <- count + 1 ; PrintError "System" p s
    static member Runtime p s = PrintError "Runtime" p s ; printfn "\nBuild Halted!!!\n" ; exit 1
    static member Bug p s = PrintError "BUG" p s ; printfn "\nBuild Halted!!!\n" ; exit 1
    static member Design p s = count <- count + 1 ; PrintError "Design" p s
    static member Build p s = count <- count + 1 ; PrintError "Build" p s
    static member Check = if count > 0 then ((printfn "\n %i Error(s) found!\nBuild Failed!!!\n" count) ; exit 1)

let IncrPos (f,l,p) : Position = let n = 1UL in (f, l , (n + p))

let IncrLine (f,l,n) : Position = (f, (l + 1UL), n)

let FileReadChars pos (s:string): ((char list) option) =
    try Some([for c in (File.ReadAllText(s)) -> c])
    with _ ->
        match pos with
        | Some(p) -> Error.Syntax p (sprintf "Unable to read the file %s!" s)
        | None -> Error.System (sprintf "Unable to read the file %s!" s)
        None

let IsWhite c = ((c = ' ') || (c = '\r')  || (c = '\t') || (c = '\n'))
let IsDigit c = ((c <= '9') && (c >= '0'))
let IsLower c = ((c <= 'z') && (c >= 'a'))
let IsUpper c = ((c <= 'Z') && (c >= 'A'))
let IsAlpha c = ((IsLower c) || (IsUpper c))
let IsAlphaNum c = ((IsAlpha c) || (IsDigit c))

let Div a b = ((a - (a % b)) / b)


let rec ConstructList pos l = // Make List
    match l with
    | h::t -> [UnionCaseTag(pos, "list"); UnionCaseData(h); UnionCaseNested(ConstructList pos t)]
    | [] -> [UnionCaseTag(pos, "nil")]


let rec DeconstructList pos d = // Extract List
    match d with
    | [] -> Error.Runtime pos "Invalid list"
    | [UnionCaseTag(_, "nil")] -> []
    | [UnionCaseTag(p, "list"); UnionCaseData(h); UnionCaseNested(dd)] -> h::(DeconstructList p dd)
    | _ ->  Error.Runtime pos "Invalid list"
