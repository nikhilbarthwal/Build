module Common

type number = uint64

type lineNum = uint64

type posNum = uint64

type fileNme = string

type pos = {LineNum:lineNum ; PosNum:posNum; FileName:string}

type posType<'T> = {Pos:pos; Data:'T}

type errorMessage = {Pos:pos; Message:string}

type nlist<'T> = 'T * ('T list)
// Get (number) : T option

type either<'T1, 'T2> = First of 'T1 | Second of 'T2
(*

type state = (block, varmap, choice point id)

let Execute(block, varmap, states)
{
    match block.split() with
    {
        some((head, block)) ->
           match head with
            | prediate ->
                create a new var map
                states += (tail, cvarMap, main pointer)
                newBlock <- get new block
                Execute(newBlock, vewVapmap, states)

            | failing statement ->
                if (choice point exist) then

                    open last choice point and coninue from there
                else
                    return None
            | passing statement ->
                newvap <- old varmap
                Execute newVar tail states


}

*)
