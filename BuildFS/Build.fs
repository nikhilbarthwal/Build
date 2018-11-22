module Build

open Program

type model = {LowerDefinitions:Map<lowerKey, lowerDefination>;
              UpperModelDefinitions:Map<upperKey, upperDefination>;
              Files:fileGenerator list;
              Errors:errorGenerator list}

type varMap = Map<string, expr * typeDef> //add(program), get(program)

type stateKey =
    | PredicareKey of PredicateName:string * Pointer:uint64
    | OrBlock of Block:predStatement option
    | No

type state = {Block:predStatement list; Vars:varMap; Key:stateKey}

type parameters = {Block:predStatement list; Vars:varMap; States:state list }

//TODO: Modify this to be Non-Tail recursive

let rec Execute (Input:parameters) : varMap option =
    let Vars = Input.Vars

    let rec NewParameters (States:state list): parameters option=
        match States with
        | [] -> None
        | h::t ->
            match h.Key with
            | PredicareKey(predicateName, pointer) ->
                if (pointer is last) then
                    NewParameters t
                else
                    Some {
                        Block = Get block from Preidicate in pointer+1
                        Vars = based on signature, create it
                        States = ({h.Block, h.Vars, pointer+1})::t
                    }
            | OrBlock(block) ->
                match block with
                | Some(x) ->
                    Some {
                        Block = x
                        Vars = h.Vars
                        States = ({h.Block, h.Vars, OrBlock(None)})::t
                    }
                | None -> NewParameters t
            | No -> NewParameters t

    match Input.Block with //Use split
    | [] -> // Block end
        match Input.States with
        | [] -> Some(Vars) // Allexecution done, continue working
        | h::t -> Execute {Block=h.Block; Vars=h.Vars; States=t} // Retrive last state and continue ...
    | h::t ->
        match h with
        | PredStatementStop -> match (NewParameters Input.States) with Some(x) -> Execute x | None -> None
        | PredStatementBlock of block<predStatement>
        | PredStatementOr of predStatement * predStatement
        | PredStatementCall of Any:bool * ActionName:upperIdentifierFull * Args:unifyExpr list
        | PredStatementIf of Condition:expr * Then:predStatement * Else:predStatement
        | PredStatementSwitch of Match:unifyExpr * Cases:caseBlock<predStatement> list * Default:predStatement option
        | PredStatementUnify of expr * expr // The "is" operator
        | PredStatementMember of Var:expr * List:expr // The "in" operator
        | PredStatementAssert of expr //Should not have "in" operator as it is a seperate statement



(*

Unify(EXpr, VarMap, bool In/Out) -> VarMap option
Eval(VarMap) -> Value

____________________________

1. Untyped evaluation
2. Types evaluation
3. Add Call stack (With Purity)
4. Add Logging

*)
