(* ***********************************************************
      TRANSFORM: Tranform program into structure
   *********************************************************** *)

module Transform

open Definations
open Utils

let private InitProgram (defs : Parse Def list) : Program =
    let Process def =
        match def with
        | DefFunc((pos,name), args, body) -> Some( (name, List.length args, pos), DefFunc((pos,name), args, body))
        | DefPred((pos,name), args, body) -> Some( (name, List.length args, pos), DefPred((pos,name), args, body))
        | DefConst((pos,name), body) -> Some ((name, 0, pos), DefConst((pos,name), body))
        | DefCustom(parseDef) ->
            match parseDef with
            | ParseNamedRule((pos,name), args, _) -> Some( (name, List.length args, pos), DefCustom([]) )
            | ParseRuleHead((pos,name), num) -> Some(( name, int num, pos), DefCustom([]))
            | ParseRule(_) -> None

    let p = defs |> List.map Process |> FilterOptions

    let AddKey state (name, number, pos) =
        let key = (name, number)
        let value =  if (Map.containsKey key state) then state.[key] else []
        state.Add(key, pos::value)

    let PrintError pos (name, number) = Error.Syntax pos (sprintf "Duplicate defination of %s/%d" name number)

    for (k,v) in (p |> List.map fst |> List.fold AddKey Map.empty |> Map.toList) do
        if ((List.length v) > 1) then (for pos in v do (Error.Syntax pos (sprintf "Duplicate defination of %s/%d" (fst k) (snd k))))

    Error.Check
    ((p |> List.map (fun ((name, arity, _), value) -> ((name, arity), value)) |> Map.ofList), [], [])

let rec private TransformRule program (predBody:(Pred Statement block)) (ruleBody:(Rule Statement block)) : Program =
    let GetBody e pos =
        let inv e =(fst e, ExprUnaryOp(OpNot, e))
        ( predBody @ [(pos, StatementAssert(e))] , predBody @ [(pos, StatementAssert(inv e))] )

    match ruleBody with
    | [] -> program
    | (pos, h)::t ->
        match h with
        | StatementAssert(e) -> TransformRule program ((pos, StatementAssert(e))::predBody) t
        | StatementStop -> TransformRule program ((pos, StatementStop)::predBody) t
        | StatementUnify(p, e) -> TransformRule program ((pos, StatementUnify(p, e))::predBody) t
        | StatementCall(i, b) -> TransformRule program ((pos, StatementCall(i, b))::predBody) t
        | StatementMember(p1, p2) -> TransformRule program ((pos, StatementMember(p1, p2))::predBody) t
        | StatementIf(e, b) ->
            let (bb1, bb2) = GetBody e pos
            let t1 = b @ t
            let prog = (TransformRule program bb1 t1) in (TransformRule prog bb2 t)
        | StatementIfElse(e, b1, b2) ->
            let (bb1, bb2) = GetBody e pos
            let t1 = b1 @ t in let t2 = b2 @ t
            let prog = (TransformRule program bb1 t1) in (TransformRule prog bb2 t2)
        | StatementSwitch(expr, cases) ->
            let ProcessCases prog (case: (Position * Rule SwitchCase)) : Program =
                let (pos2, (pattern, optExpr, b)) = case
                let p = predBody @ [(pos, StatementUnify((pos2, pattern), expr))]
                let pp = match optExpr with Some e -> (p @ [(pos2, StatementAssert(e))]) | None -> p
                TransformRule prog pp (b @ t)
            List.fold ProcessCases program cases
        | StatementCustom(custom) ->
            match custom with
            | RuleStatementAnd(l) -> List.fold (fun s x ->  (TransformRule s predBody (x @ t))) program l
            | RuleStatementExport(name, args) ->
                let n = List.length args
                let s = snd name
                let (map, err, file) = program
                if (Map.containsKey (s,n) map ) then
                    match map.[(s,n)] with
                    | DefCustom(l) -> let lh:Transform = (args,predBody) in (map.Add((s,n), DefCustom(lh::l)),  err, file)
                    | _ -> Error.Syntax pos (sprintf "There is other defination for %s/%d" s n) ; program
                else
                    Error.Syntax pos (sprintf "There is no defination for %s/%d" s n) ; program
            | RuleStatementDesignError(expr) -> let (map, err, file) = program in (map, (pos, expr, predBody)::err, file)
            | RuleStatementFileGenerate(name, content) -> let (map, err, file) = program in (map, err, ((pos, name, content, predBody)::file))

let private CustomFilter def = match def with DefCustom(T) -> Some(T) | _ -> None

let private TransformFunc (program:Program) (def:Parse) : Program =
    match def with
    | ParseRule(body) -> TransformRule program [] body
    | ParseNamedRule(name, args, body) -> program
    | ParseRuleHead(_, _) -> program

let Main defs : Program = defs |> List.map CustomFilter |> FilterOptions |> List.fold TransformFunc (InitProgram defs)
