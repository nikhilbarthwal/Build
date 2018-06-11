(*
program = pos{def}*.

    def =
        defFunc(pos{string} Name, pos{string}* Params, pos{statement}* Body);
        defPred(pos{string} Name, pos{string}* Params, pos{statement}* Body).

    param =
        paramConst(pos{expr});
        paramUnify(pos{expr});
        paramIgnore.

    statement =
        statementIf(pos{expr} Condition, pos{statement}* Body);
        statementIfElse(pos{expr} Condition, pos{statement}* BodyThen, pos{statement}* BodyElse);
        statementUnify(pos{expr} ExprLeft, pos{expr} ExprRight);
        statementCall(pos{string} PredName, pos{param};
        %statementAssert(pos{expr});
        %statementContinue(pos{expr});
        statementReturn(pos{expr});
        %statementYield(pos{expr});
        statementStop.

    expr =
        exprIf(pos{expr} Condition, pos{expr}* ExprThen, pos{expr}* ExprElse);
        exprStr(pos{string});
        exprNum(pos{integer});
        exprNum(pos{bool});
        exprNegate(pos{expr});
        exprList(pos{expr}* );
        exprVar(pos{string});
        exprAdd(pos{expr},pos{expr});
        exprSub(pos{expr},pos{expr});
        exprMult(pos{expr},pos{expr});
        exprDiv(pos{expr},pos{expr});
        exprConcat(pos{expr},pos{expr});
        exprAppend(pos{expr},pos{expr});
        exprJoin(pos{expr},pos{expr});
        exprCall(pos{string},pos{expr}* ).
        exprEq(pos{expr},pos{expr});
        exprNeq(pos{expr},pos{expr});
        exprGt(pos{expr},pos{expr});
        exprLt(pos{expr},pos{expr});
        exprGe(pos{expr},pos{expr});
        exprLe(pos{expr},pos{expr});
        exprAnd(pos{expr},pos{expr});
        exprOr(pos{expr},pos{expr});
        exprNot(pos{expr},pos{expr});
        exprIn(pos{expr}).

// ***********************************************************
//    DEFINATIONS.FS: All the definations for the program
// ***********************************************************
//
module Definations

// ______________________________________________________________________________________
//   position: Position to track tokens
type position = string * uint32 * uint32 // FileName, LineNo, Position

// ______________________________________________________________________________________
//   Identifier: Consists of string chat, one with small letter - other with large
type Identifier = position * string // String should be upper identifier

type identifier = position * string // String should be lower identifier

// ______________________________________________________________________________________
//   moduleIdentifier: Identifier to track items in modules
type moduleIdentifier = 
    | IdentifierThis of Identifier
    | IdentifierThat of Identifier * Identifier

// ______________________________________________________________________________________
//   expr: Expression
type expr = 
    | ExprNumber of position * int64 // Number
    | ExprString of position * string // String
    | ExprBool of position * bool // String
    | ExprList of position * expr list // List [1, 2, 3, ..]
    | ExprTailList of position * expr list * expr // [1, 2, 3 | X]
    | ExprVar of moduleIdentifier // Variable
    | ExprTuple of position * expr list // {identifier}({expr} ...)
    | ExprTerm of identifier * expr list
    | ExprFunCall of moduleIdentifier * expr list // {identifier}({expr} ...)
    | ExprRecord of position * Map<Identifier, expr>
    | ExprMapCall of moduleIdentifier * expr
    | ExprAdd of expr * expr // {expr1} + {expr2}
    | ExprSub of expr * expr // {expr1} - {expr2}
    | ExprMult of expr * expr // {expr1} * {expr2}
    | ExprDiv of expr * expr // {expr1} / {expr2}
    | ExprMod of expr * expr // {expr1} % {expr2}
    | ExprConcat of expr * expr // String concatenation
    | ExprJoin of expr * expr // {expr1} is head and {expr2} is tail
    | ExprAddList of expr * expr // Adding two list
    | ExprNegate of position * expr // -{expr}
    | ExprIfThen of position * expr * expr // if {cond} then {expr1} else {expr2}
    | ExprIfThenElse of position * expr * expr * expr // if {cond} then {expr1} else {expr2}
    | ExprMemberCheck of expr * expr // {expr1} in {expr2}
    | ExprAnd of expr * expr // {cond1} and {cond2}
    | ExprOr of expr * expr // {cond1} or {cond2}
    | ExprNot of position * expr // not {cond}
    | ExprGtEq of expr * expr // {expr1}>={expr2}
    | ExprGt of expr * expr // {expr1}>{expr2}
    | ExprLtEq of expr * expr // {expr1}<={expr2}
    | ExprLt of expr * expr // {expr1}<{expr2}
    | ExprNotEq of expr * expr // {expr1}!={expr2}
    | ExprEq of expr * expr // {expr1}={expr2}

// ______________________________________________________________________________________
//   arg: Arguments to Predicate
type arg = 
    | ArgExpr of expr
    | ArgIgnoreVar of Identifier
    | ArgIgnore
    | ArgOutput of expr

// ______________________________________________________________________________________
//   statement: Statement of a body
type statement = 
    | StatementIfThenElse of position * expr * block * block // if {cond} then {...} else {...}
    | StatementIfThen of position * expr * block // if {cond} then {statement...}
    | StatementUnify of position * expr * expr // {expr1} = {expr2}
    | StatementYeild of position * expr // yeild {expr}
    | StatementCall of position * moduleIdentifier * arg list // Predicate call
    | StatementContinue of position * expr // continue {expr}
    | StatementMember of position * expr * expr
    | StatementAssert of position * expr // Assert {cond}
    | StatementReturn of position * expr // Return {expr}
    | StatementExport of position * expr * moduleIdentifier // Export {Identifier0} to {Identifier1}
    | StatementExportKey of position * expr * moduleIdentifier * expr // Export {I0} to {Id} with {key}
    | StatementSwitchNoDefault of position * expr * switchCase list // Switch cases, no default
    | StatementSwitchDefault of position * expr * switchCase list * block // Switch cases
    | StatementGenerate of position * expr * block // {expr} = Statement block with Yeild
    | StatementLoopDef of position * expr * Identifier * expr * block // {expr} = Init ({expr})
    | StatementStop of position // stop statement

//   switchCase: Switch case for switch case
and switchCase = expr * expr * block

//   block: Program definations included in the source file
and block = position * position * (statement list)

// ______________________________________________________________________________________
//   typedef: Types used to represent expressions
type typeDef = 
    | TypeUnion of union list
    | TypeTuple of typeDef list
    | TypeList of typeDef
    | TypeRecord of Map<Identifier, typeDef>
    | TypeReference of string * string
    | Number
    | String
    | Bool
    | Unknown

//   union: Used to represent discrimated union
and union = identifier * typeDef list

//   record: Used to represent a record pair
and record = Identifier * typeDef


// ______________________________________________________________________________________
//   eval: Evaluated Expression
type eval = 
    | EvalNumber of int64 // Number
    | EvalString of string // String
    | EvalBool of bool // String
    | EvalList of value list // List [1, 2, 3, ..]
    | EvalVar of Identifier // Variable
    | EvalTuple of value list // {identifier}({expr} ...)
    | EvalTerm of identifier * value list
    | EvalRecord of Map<Identifier, value>

//   value: Used to represent a type and eval pair
and value = typeDef * eval


// ______________________________________________________________________________________
//   param: Functional paremet which could be typed or untyped
type param = 
    | ParamUntyped of Identifier
    | ParamTyped of Identifier * typeDef

// ______________________________________________________________________________________
//   defination: Program definations included in the source file
type defination = 
    | DefineCond of Identifier * param list * block // Condition defination
    | DefineFunc of Identifier * param list * block // Function defination
    | DefinePred of Identifier * param list * block // Predicate defination
    | DefineStart of block // Start Predicate defination
    | DefineConst of param * expr // Constant of expr
    | DefineVar of Identifier * typeDef // Variable of type Module Identifier
    | DefineMap of Identifier * typeDef * typeDef // key, value type
    | DefineArray of Identifier * typeDef // List of type Module Identifier
    | DefineType of Identifier * typeDef // Types definations

// ______________________________________________________________________________________
//   defination: Program definations included in the source file
type moduleDef = string * Identifier * defination list

// ______________________________________________________________________________________
//   defination: Program definations included in the source file
type program = position * string * moduleDef list


// ______________________________________________________________________________________
//   defination: Program definations included in the source file
type definationType = 
    | DefConstant of value
    | DefPredicate of param list * block // Predicate
    | DefFunction of param list * block // Function
    | DefCondition of param list * block // Condition
    | DefSingleVar of typeDef // of Type 
    | DefMapVar of typeDef * typeDef // Map with key type and value type
    | DefListVar of typeDef
    | DefType of typeDef

// ______________________________________________________________________________________
// defination: Program definations included in the source file
type varBag = Map<string, value>

// ______________________________________________________________________________________
// unifyArge: Argument to Unify
type unifyArg =
| UnifyArgExpr of expr
| UnifyArgValue of value

// ______________________________________________________________________________________
//   predResult: Result of Pred block ot statement evaluation
type predResult = varBag list option

// ______________________________________________________________________________________
//   funcResult: Result of Function block ot statement evaluation
type funcResult = 
    | FuncResultVars of varBag // Should not happen in Block!
    | FuncResultReturn of value

// ______________________________________________________________________________________
//   seqResult: Result of Generate block ot statement evaluation
type seqResult = (value list) * ((varBag list) option) // Block will return value list

// ______________________________________________________________________________________
//   loopResult: Result of Generate block ot statement evaluation
type loopResult = 
    | LoopResultContinue of value
    | LoopResultYield of value
    | LoopResultVars of varBag

*)
