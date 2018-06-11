module Common

type number = uint64

type lineNum = uint64

type posNum = uint64

type fileName = string

//type pos = {LineNum:lineNum ; PosNum:posNum; FileName:string}
type pos = {LineNum:lineNum ; PosNum:posNum}
type posType<'T> = {Pos:pos; Data:'T}

//type errorMessage = {Pos:pos; Message:string}

type nlist<'T> = 'T * ('T list)
// Get (number) : T option
// Find (x) : bool
// Pos(x) : number


type either<'T1, 'T2> = First of 'T1 | Second of 'T2

(*
type loopType<'U, 'V> = Continue of 'U | Return of 'V

//NOT IN USE
let rec RunFold (f:'S->'T->loopType<'S, 'S>) (s0:'S) (l:'T list): 'S =
    match l with
    | h::t -> match (f s0 h) with Continue(s1) -> RunFold f s1 t | Return(r) -> r
    | [] -> s0

//NOT IN USE
let rec RunLoop<'U, 'V>  (f:'U->loopType<'U,'V>) (state:'U) : 'V =
    match (f state) with
    | Continue(s) -> RunLoop f s
    | Return(r) -> r
*)