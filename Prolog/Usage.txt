 USAGE
_______

In Statement as while :-

v1 = expr1
v2 = expr2
v3 = expr3
while PredCall:
    determ statement block


in code, that translate to:

let While(PredCall, Block, v0: StackedVarsDictionary): Maybe<Exception> =

    let Eval stream: LoopVar[Maybe[exception], Stream] =
        march stream.Next() with
        | Error(e) -> Return(Yes(e))
        | Ok(No) -> Return(No)
        | Ok(Yes(v)) -> match block.Eval(v) with
                        | No -> Continue(stream)
                        | Yes(e) -> Return(Yes(e))

    PredCall.GetStream(v0: VarMap).HandleError(Loop.While Eval)



In if/then/else statement:-

if (once PredCall) then BTrue else BFalse

in code, that translate to:

let ifThenElseStatement(PredCall, BTrue, BFalse, vars): Maybe[Exception] =

    let evalVars (v: Maybe[Vars]): Maybe[Exception] =
        match v with Yes(y) -> BTrue.Eval(y) | No -> BFalse.Eval(x)

    let evalStream stream: Maybe[Exception] =
        stream.Next() |> HandleError (Apply evalVars)

    PredCall.GetStream(vars).HandleError <| ApplyMaybe(evalStream)


let ifThenElseExpression(PredCall, BTrue, BFalse, vars): Result[LazyValue, Type] =

    let evalVars (v: Maybe[vars]): Result[LazyValue, Type] =
        match v with Yes(y) -> BTrue.Eval(y) | No -> BFalse.Eval(vars)

    let evalStream stream: Result[LazyValue, Type] =
        stream.Next().HandleResult(evalVars)

    let eval (z: Maybe[Stream]): Result[LazyValue, Type] =
        match z with Yes(s) -> (evalStream s) | No -> BFalse.Eval(vars)

    PredCall.GetStream(vars).HandleError(eval)
