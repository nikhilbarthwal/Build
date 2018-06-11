module Generate


type state = Map<string, constExpr> // Map(Name, constExpr)

type const = // Output of Eval
    | EvalConstantStr of string
    | EvalConstNum of integer
    | EvalConstBool of bool
    | EvalConstUnion of (unionEvalConst list)
    | EvalConstFunction
    | EvalConstPredicate
    | EvalConstError

and unionEvalConst
    | UnionEvalConstTag of string (* String should be lower case *)
    | UnionEvalConstExpr of constant

// The expr should be completely bounded, Will raise a Runtime expcetion if not the case!
let EvalExpr (Vars:state) ((Pos, Expr) : position * varExpr) : evalConst = EvalConstError // TO DO: Finish this EvalConstFunction

//let EvalIntExpr (Vars:state) ((Pos, Expr) : position * varExpr) : evalConst = EvalConstError // TO DO
//let EvalListExpr (Vars:state) ((Pos, Expr) : position * varExpr) : evalConst = EvalConstError // TO DO
//let EvalBoolExpr (Vars:state) ((Pos, Expr) : position * varExpr) : evalConst = EvalConstError // TO DO

// Add a Variable to state table and raise a runtime exception if it already exists
let AddVar (Vars:state) (Name:identifier) (Value:constant) = Vars

// UnifyConditionCheck: If == then Unify otherwise UnifyConditionCheck



// Needs to Unify confition - Can be Determ or Non-Determ
