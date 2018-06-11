(* ***********************************************************
      EXECUTE.FS: Validate all program definations
  ************************************************************ *)

module Execute

open Definations
open Utils

let private CheckData (pos:Position) (p1, e1) (p2, e2) : bool = //Check if the expr are equal or not
    match (e1, e2) with
    | (DataFunction(_, _), _) -> Error.Runtime p1 "Comparisons of Functions are not permitted!"
    | (_, DataFunction(_, _)) -> Error.Runtime p2 "Comparisons of Functions are not permitted!"
    | (DataChar(c1), DataChar(c2)) -> c1 = c2
    | (DataBool(b1), DataBool(b2)) -> b1 = b2
    | (DataNum(n1), DataNum(n2)) -> n1 = n2
    | (DataUnion(l1), DataUnion(l2)) -> l1 = l2
    | (_, _) -> Error.Runtime pos "Type mismatch in comparisons"

let private AddVar (program: ProgramMap) (state:State) (position, name) data : State =
    if ( (state.ContainsKey(name)) || (program.ContainsKey(name,0)) ) then Error.Runtime position (sprintf "Attempt to re-define of %s" name)
    state.Add(name, data)

let private GetVar (program: ProgramMap) (state:State) (position, name) =
    if (state.ContainsKey(name)) then state.[name]
    else
        let key = (name,0)
        if program.ContainsKey(key) then
            match (program.[key]) with
            | DefConst((p, nameConst), (_, data)) -> if (nameConst = name) then data else Error.Bug p "Wrong mismatch of the key"
            | _ -> Error.Bug position "Wrong mismatch of the key"
        else
            Error.Runtime position (sprintf "Symbol %s not defined" name)

let rec private EvalExpr (program: ProgramMap) state (pos, expr) : Data =
    let Num = EvalNum program state
    let Bool = EvalBool program state
    let Str = EvalStr program state
    let Expr = EvalExpr program state

    let Call nameCall (argsCall:Expr block) =
        let newState argsCall args = List.zip argsCall args |> List.fold (fun state (aa, arg) -> AddVar program state arg (Expr aa)) Map.empty
        let keyCall = (nameCall, argsCall.Length)
        if (program.ContainsKey(keyCall)) then
            match (program.[keyCall]) with
            | DefFunc((_, nameFunc), argsFunc, exprFunc) ->
                if (nameFunc <> nameCall) then Error.Bug pos (sprintf "Names/Key %s & %s do not match" nameFunc nameCall)
                if (argsCall.Length = argsFunc.Length) then (EvalExpr program (newState argsCall argsFunc) exprFunc)
                else Error.Bug pos "Arguments Number does not match"
            | DefPred((_, namePred), argsPred, bodyPred) ->
                if (namePred <> nameCall) then Error.Bug pos (sprintf "Names/Key %s & %s do not match" namePred nameCall)
                if (argsCall.Length <> argsPred.Length) then Error.Bug pos "Arguments Number does not match"
                DataBool(match (EvalBlock program (newState argsCall argsPred) bodyPred) with Some(_) -> true | None -> false)
            | DefConst((_, nameConst), (_, data)) ->
                if (nameConst <> nameCall) then Error.Bug pos (sprintf "Names/Key %s & %s do not match" nameConst nameCall)
                if (argsCall.Length = 0) then Error.Bug pos (sprintf "Constants can't be mapped to keys %s/%d  > 0" nameConst argsCall.Length)
                data
            | DefCustom(_) -> Error.Runtime pos (sprintf "Cannot call a Rule %s/%d from Function"  nameCall argsCall.Length)
        else
            Error.Runtime pos (sprintf "No defination for %s/%d exists" nameCall argsCall.Length)

    match expr with
    | ExprIf(e, e1, e2) -> if (Bool e) then (Expr e1) else (Expr e2)
    | EvalValue(v) -> v
    | EvalVar(v) -> GetVar program state v
    | ExprEval(nameExpr, argsCall) -> Call (Str nameExpr) argsCall
    | ExprCall((_,nameCall), argsCall) -> Call nameCall argsCall
    | ExprUnaryOp(op, e) ->
        match op with
        | OpNegate -> DataNum(-1 * (Num e))
        | OpNot -> DataBool(not (Bool e))
    | ExprUnion(l) ->
        let rec Process case : (Data unionCase) =
            match case with
            | UnionCaseData(e) -> UnionCaseData(Expr e)
            | UnionCaseTag(m) ->  UnionCaseTag(m)
            | UnionCaseNested(ee) -> UnionCaseNested(List.map Process ee)
        DataUnion(List.map Process l)

    | ExprLet(l, ex) -> EvalExpr program (List.fold (fun s (i, e) -> (AddVar program s i (EvalExpr program state e))) state l) ex

    | ExprSeq(b, p) ->
        match (EvalBlock program state b) with
        | None -> DataUnion([UnionCaseTag(pos, "nil")])
        | Some(l) -> DataUnion(ConstructList pos (List.map (fun x -> EvalExpr program x p) l))
    | ExprBinaryOp(op, e1, e2) ->
        let Check p1 p2 : bool = CheckData pos (fst p1, (Expr p1)) (fst p2, (Expr p2))

        match op with
        | OpAdd -> DataNum((Num e1) + (Num e2))
        | OpSub -> DataNum((Num e1) - (Num e2))
        | OpMult -> DataNum((Num e1) * (Num e2))
        | OpDiv -> DataNum(Div (Num e1) (Num e2))
        | OpAppend ->
            let p1 = (fst e1) in let p2 = (fst e2)
            let u1 = match (Expr e1) with DataUnion(x) -> x | _ -> Error.Runtime p1 "Invalid List!"
            let u2 = match (Expr e2) with DataUnion(x) -> x | _ -> Error.Runtime p2 "Invalid List!"
            DataUnion(ConstructList pos (List.concat [DeconstructList p1 u1; DeconstructList p2 u2]))
        | OpNeq -> DataBool(not (Check e1 e2))
        | OpGt -> DataBool((Num e1) > (Num e2))
        | OpGe -> DataBool((Num e1) >= (Num e2))
        | OpLt -> DataBool((Num e1) < (Num e2))
        | OpLe -> DataBool((Num e1) <= (Num e2))
        | OpAnd -> DataBool((Bool e1) && (Bool e2))
        | OpOr -> DataBool((Bool e1) || (Bool e2))
        | OpEq -> DataBool(Check e1 e2)
        | OpIn ->
            let p = (fst e2) in let u = match (Expr e2) with DataUnion(x) -> x | _ -> Error.Runtime p "Invalid List!"
            let l = DeconstructList p u in let d = (Expr e1)
            if (List.exists (fun x -> x=d) l) then DataBool(true) else DataBool(false)
    | ExprSwitch(e, l, o) ->
        let rec Select z l = match l with [] -> o | (c, b) ::t -> if (CheckData pos z (fst c, Expr c)) then b else (Select z t)
        Expr (Select (fst e, Expr e) l)

and private EvalBool  program state e =
    match (EvalExpr program state e) with
    | DataBool(b) -> b
    | _ -> Error.Runtime (fst e) "Expression here should evaluate to a boolean"

and private EvalNum program state e: int =
    match (EvalExpr program state e) with
    | DataNum(n) -> n
    | _ -> Error.Runtime (fst e) "Expression here should evaluate to a number"

and private EvalStr program state e : string =
    let p = (fst e) in let d = (EvalExpr program state e)
    let u = DeconstructList p (match d with DataUnion(x) -> x | _ -> Error.Runtime p "Expression should char list")

    let rec Process l =
        match l with
        | [] -> []
        | DataChar(c)::t -> c::(Process t)
        | _ -> Error.Runtime p "Expression should char list"

    new string [|for c in (Process u) -> c|]

and private EvalBlock program state (body: Pred Statement block) : States =

    let CombineStates x = let z = (FilterOptions x) in if (z = []) then None else Some(List.concat z)
    let Bool = EvalBool program state
    let Expr = EvalExpr program state
    let Eval = EvalBlock program state
    let Evals states body : States =
        match states with
        | None -> None
        | Some(x) -> x |> List.map (fun s -> EvalBlock program s body) |> CombineStates

    let rec EvalPattern pos p : Data =

        let rec EvalPatternUnion pos (u : Pattern unionCase) =
            match u with
            | UnionCaseData(z) -> UnionCaseData(EvalPattern pos z)
            | UnionCaseTag(tag) -> UnionCaseTag(tag)
            | UnionCaseNested(l) -> UnionCaseNested(List.map (EvalPatternUnion pos) l)

        match p with
        | PatternVar(e) -> EvalExpr program state e
        | PatternIgnore -> Error.Runtime pos "Should not have ignore expr here"
        | PatternUnify(_, n) -> Error.Runtime pos (sprintf "Should not unification var %s" n)
        | PatternUnion(l) -> DataUnion(List.map (EvalPatternUnion pos) l)

    let rec GroundPattern pos pattern = // Check if does not contain any ?

        let rec GroundPatternUnion pos (u : Pattern unionCase) =
            match u with
            | UnionCaseData(z) -> GroundPattern pos z
            | UnionCaseTag(tag) -> ()
            | UnionCaseNested(l) -> List.iter (GroundPatternUnion pos) l

        match pattern with
        | PatternVar(e) -> ()
        | PatternIgnore -> Error.Runtime pos "Should not have ignore expr here"
        | PatternUnify(_, n) -> Error.Runtime pos (sprintf "Should not unification var %s" n)
        | PatternUnion(l) -> List.iter (GroundPatternUnion pos) l

    let rec UnifyPattern localState (pos1, pattern1) (pos2, pattern2) : State option =

        let rec ConvertPattern (x: pExpr unionCase) : Pattern unionCase =
            match x with
            | UnionCaseData(z) -> UnionCaseData(PatternVar(z))
            | UnionCaseTag(tag) -> UnionCaseTag(tag)
            | UnionCaseNested(l) -> UnionCaseNested(List.map ConvertPattern l)

        let UnifyPatternExpr s u e =
            match e with
            | (p, ExprUnion(l)) -> UnifyPattern s u (p, PatternUnion(List.map ConvertPattern l))
            | _ -> None

        let rec UnifyPatternUnionCase stateOpt ((u1: Pattern unionCase), (u2: Pattern unionCase)): State option =
            match stateOpt with
            | None -> None
            | Some(s) ->
                match (u1, u2) with
                | (UnionCaseTag(_, t1), UnionCaseTag(_, t2)) -> if (t1 = t2) then stateOpt else None
                | (UnionCaseData(uu1), UnionCaseData(uu2)) -> UnifyPattern s (pos1, uu1) (pos2, uu2)
                | (UnionCaseNested(l1), UnionCaseNested(l2)) -> List.zip l1 l2 |> List.fold UnifyPatternUnionCase stateOpt
                | (_, _) -> None

        match (pattern1, pattern2) with
        | (PatternUnify(n) , e) -> Some(AddVar program state n (EvalPattern pos2 e))
        | (e, PatternUnify(n)) -> Some(AddVar program state n (EvalPattern pos1 e))
        | (PatternIgnore, e) -> GroundPattern pos2 e ; Some(state)
        | (e, PatternIgnore) -> GroundPattern pos1 e ; Some(state)
        | (PatternVar(e1), PatternVar(e2)) ->
            let d1 = (EvalExpr program state e1) in let d2 = (EvalExpr program state e2)
            if (CheckData pos1 (pos1, d1) (pos2, d2)) then Some(state) else None
        | (PatternUnion(l1), PatternUnion(l2)) -> List.zip l1 l2 |> List.fold UnifyPatternUnionCase (Some(state))
        | (PatternUnion(u), PatternVar(e)) -> UnifyPatternExpr state (pos1, pattern1) e
        | (PatternVar(e), PatternUnion(u)) -> UnifyPatternExpr state (pos2, pattern2) e

    match body with
    | [] -> Some([state])
    | (pos, h) :: t->
        match h with
        | StatementAssert(e) -> if (Bool e) then (Eval t) else None
        | StatementStop -> None
        | StatementIf(e, b) -> if (Bool e) then (Evals (Eval b) t) else (Eval t)
        | StatementIfElse(e, b1, b2) -> if (Bool e) then (Evals (Eval b1) t) else (Evals (Eval b2) t)
        | StatementUnify(u1, u2) -> match (UnifyPattern state u1 u2) with Some(s) -> (EvalBlock program s t) | None -> None
        | StatementSwitch(u, bb) ->
            let ProcessCaseBlock (pos, (pattern, exprOpt, blk)) : States =
                match (UnifyPattern state u (pos, pattern)) with
                | Some(newState) ->
                    let proceed = match (exprOpt) with Some(e) -> (EvalBool program newState e) | None -> true
                    if (proceed) then (EvalBlock program newState blk) else None
                | None -> None
            bb |> List.map ProcessCaseBlock |> CombineStates
        | StatementMember((pu1,u1), (pu2,u2)) ->
            match u2 with
            | PatternUnion(pl) ->
                let res = DeconstructList pu2 pl |> List.map (fun x -> (pu2, x)) |> List.map (UnifyPattern state (pu1,u1)) |> FilterOptions
                if (res = []) then None  else Some(res)
            | _ -> Error.Runtime pu2 "This expression should be a list"
        | StatementCustom(PredStatementOr(bb)) -> bb |> List.map Eval |> CombineStates
        | StatementCall(name, ul) -> // %%% TO DO %%%
            let key = (name, ul.Length)
            if program.ContainsKey(key) then
                let entity = program.[key]

            else Error.Runtime pos (sprintf "No defination for %s/%d exists" (snd key))
