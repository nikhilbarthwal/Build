module Build

open Common
open Definations

let MaxErrors:number = 1000UL
let MaxFiles:number = 1000UL

// type logStatement = string //TODO: {FormatString:string; ExprList:expr list; Indent:number}
// type logs = logStatement list

type varMap = Map<string, expr * typeDef> //FIXME: Should I merger it with Context?
//let NewVarMap:varMap = Map.empty FIXME: Should I remove it?

//TODO: Declare call stack here
type exp = string // TODO: Should contain message + full call stack
type execution<'T> = Ok of 'T  | Exception of exp

type eval = {Value:constant; Type:typeDef} with
    static member SameType (e1:eval) (e2:eval) : bool = true //TODO: Complete here
    static member Equal (e1:eval) (e2:eval) : execution<bool> = Ok(true) //TODO: Complete here

type program = {LowerDefinitions:Map<lowerKey, inContext<lowerDefination>>; // TODO: Make it a class
                UpperModelDefinitions:Map<upperKey, inContext<upperDefination>>} with
                //TODO: Add Desugar a block here
    member this.EvalExpr (varMap:varMap) (context:nameSpace) (expr:expr) (identifier:upperIdentifier): execution<eval> = Exception("") //TODO: Complete
    member this.AddVar (varMap:varMap) (context:nameSpace) (identifier:upperIdentifier) (constant:constant): execution<varMap> = Exception("") //TODO: Complete
    member this.GetVar (varMap:varMap) (context:nameSpace) (identifier:upperIdentifier): execution<constant> = Exception("") //TODO: Complete
    member this.Unify (varMap:varMap) (context:nameSpace) (partial:bool) (pattern:patternExpr) (constant:constant): execution<varMap> = Exception("") //TODO: Complete
    member this.Execute block context (count:number) (action:varMap -> exp option) (max:number): execution<(varMap * choicePoints) option> = Exception("") //TODO: Complete
        (*
Mapper(Transition, VarMap) -> exception<VampMap option>
Choicepoint = (Block, VarMap, Transition)
Action: (X,VarMap) -> exception<loopType<X, bool>>
    exception(e) : Exception happens
    continue(x) : Operation successfull and continue working
    Return(x) : If the limit has reached

Member will: ChoicePoints = [], V = Map.empty

Fun(Block, V:VarMap, ChoicePoints, X, Action): exception<loopType<X, X>>

    match Block with
    | [] ->
        match ChoicePoints with
        | [] -> return Action(X,V)
        | (Block1, VarMap1, Transition1)::T ->
            match Mapper(Transistion,V) with
            | Ok(V1) -> Fun(Block1, V1, T, X)
            | Exception(e) -> Exception(e)
    | H::T ->

        let BlockHelper(X, Block): exception<loopType<X, bool>> =
            VV = evaluate
            Transition = get it from current call
            Choicepoint1 = C(Block, Transition, V)
            Fun(NewBlock, VV, Choicepoint1::Choicepoint)

        if H is Predicate:
            RunLoopWithException (BlockHelper X) (All blocks in Predicate)

        if H is Or(B1,B2):
            match BlockHelper(X,B1) with
            | Exception(e) -> Exception(Z)
            | Return(Z) -> Return(Z)
            | Continue(Z) -> BlockHelper(Z,B2)

        if H is fail:
            return Ok(ContinueX))

        if H is pass then:
            VV = evaluate
            return Fun(T,VV,ChoicePoints)


exception(e) : Exception happens
continue(0) : if blocks were emptry then WARNING else ERROR
continue(N) : N files generated
Return(X) : If the limit has reach
        // L = block, V is initially empty
        Fun(L, V, C, Action, Max): exception<number>
            match L with
            | [] ->
                match Action(V)
                | some(e) -> Exception(e)
                | none -> Ok(C+1)

            | H::T ->
                if H is Predicate:
                    let FunHelp (LL:nlist) C0: exception<number>
                        if (C0 >= Max) then
                            return C0
                        else
                            let HH::TT = LL
                                VV = evaluate
                                match Fun(Desugar HH,VV,C0) with
                                | Ok(C1) -> match TT with h::t -> (FunHelp  (h,t) C1) else Ok(C1)
                                | Exception(e) -> Exception(e)
                    in FunHelp (All solutions oh H) C

                if H is fail: Ok(C)

                if H is pass then:
                    VV = evaluate
                    Fun(T,VV,C)
        *)

let ExecuteModel (program:program) (files:inContext<fileGenerator> list) (errors:inContext<errorGenerator> list): int =

    let GenerateErrors (count:number) (e:inContext<errorGenerator>) : loopType<number, execution<number>> =
        let Action (v:varMap):exp option = None //TODO: Complete this
        match (program.Execute (Desugar x.Item.Block) x.Context Map.empty count Action MaxErrors) with
        | Ok(errors) -> if (errors >= MaxErrors) then Return(Ok(errors)) else Continue(errors)
        | Exception(e) -> Return(Exception(e))

    let GenerateFiles (count:number) (x:inContext<fileGenerator>) : loopType<number, execution<number>> =
        let Action (v:varMap):exp option = None //TODO: Complete this
        match (program.Execute (Desugar x.Item.Block) x.Context Map.empty count Action MaxFiles) with
        | Ok(files) -> if (files >= MaxFiles) then Return(Ok(files)) else Continue(files)
        | Exception(e) -> Return(Exception(e))

    let rec RunFoldWithException (f:'S->'T->loopType<'S, execution<'S>>) (s0:'S) (l:'T list): execution<'S> =
        match l with
        | h::t -> match (f s0 h) with Continue(s1) -> (RunFoldWithException f s1 t) | Return(r) -> r
        | [] -> Ok(s0)

    match (RunFoldWithException GenerateErrors 0UL errors) with // Check Design Issues
    | Ok(0UL) ->
        match (RunFoldWithException GenerateFiles 0UL files) with
        | 0 -> if files.length = 0 then "WARNING! Nothing to do!" else "ERROR:Everything failed"
        | 1 -> printfn " SUCCESS: %s generated!" msg
        | n -> if (n>=MaxFiles) then (sprintf "%d files" n) else (sprintf "max %d files" n)
        | Exception(exp) -> (* Deal With Exception exp *) 1

    | Ok(1UL) -> printfn " FAILURE: one design issue found!" ; 1
    | Ok(e) when (e > MaxErrors) -> printfn " FAILURE: More than %d design issues found!" MaxErrors ; 1
    | Ok(e) -> printfn " FAILURE: %d design issues found!" e ; 1
    | Exception(exp) -> (* Deal With Exception exp *) 1

(*------------------------------------------

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
        | [] -> Some(Vars, statelist) // All execution done, continue working
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

*)
