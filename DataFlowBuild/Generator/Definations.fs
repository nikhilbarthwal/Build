module Definations

open System
open Utils
open Templates

type args = string * string // Type, Name
type union = string * string * args list // Tag, Description, Args 
type typeDefine = 
    | Union of string * string * union list// Name, Description, UnionsCases
    | Tuple of string * string * args list // Name, Description, Args 
    | Record of string * string * args list // Name, Description, Args 

type defination =
    | Singleton of typeDefine
    | Chain of typeDefine list

let FileData =
    let moduleName = "Definations" 
    let fileName = moduleName + ".fs"
    let description = "All definations are contained here"
    let data = (moduleName, fileName, description)
    (data, FileHeader data)

let Definations =
    // TODO: For now there are no types
    let program = Record("program", "Structure for the Program", [("module list", "Modules")] )
    let moduleDef = Tuple("defNamespace", "Hold the Namespace", [("string","Name"); ("defDeclation list", "Declarations")] )
    let constDec : union = ("defConst", "Constant Declaration", [("Identifier", "Name") ; ("defType option","Type") ; ("exprConst","Value")] )
    let defineDef = Union("defDefine", "Declaration", [ constDec ] ) 
    let position = Tuple("position", "Position to track tokens", [("string","FileName"); ("uint32","LineNo"); ("uint32","Position") ] )
    let Identifier = Tuple("Identifier", "Upper Identifier", [("position","Position"); ("string","Name") ] )
    let identifier = Tuple("identifier", "Lower Identifier", [("position","Position"); ("string","Name") ] )
    [Singleton(position); Singleton(identifier); Singleton(Identifier) ; Singleton(defineDef) ; Singleton(program) ]

(*

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

let PrintDefination : fileItem =
    let printUnion (p:string) ((n,d,a):union) =
        let s1 = "| " + p + n + " of "
        let s2 = (fst (List.head a )) + " "
        let s3 = (List.tail a) |> List.map (fun x -> " * " + (fst x))|> String.Concat
        let ch = (snd (List.head a ))     
        let cl = (List.tail a) |> List.map (fun x -> ", " + (snd x)) |> String.Concat
        let s4 = p + n + "(" + ch + cl + ") : " + d
        Line2Str(s1 + s2 + s3, s4, Tab, "")

    let printDefination (b:bool) (def:typeDefine) =
        match def with
        | Union(n,d,l) ->
            let s = [LineStr((if b then "type " else "and ") + n , "", "") ] |> List.append ( TypeHeader n d) 
            Spacing |> List.append (l |> List.map (printUnion n) |> List.append s)
        | Tuple(n,d,l) ->
            let h = n + "(" + (List.head l |> fst) + ( List.tail l |> List.map (fun x -> ", " + (snd x) ) |> String.Concat ) + ")"
            let s = "type " + n + " = " + (List.head l |> fst) + ( List.tail l |> List.map (fun x -> " * " + (fst x) ) |> String.Concat )
            Spacing |> List.append [LineStr(s,"","")] |> List.append ( TypeHeader h d)
        | Record(n,d,l) ->
            let h = n + " {" + (List.head l |> fst) + ( List.tail l |> List.map (fun x -> ", " + (snd x) ) |> String.Concat ) + "}"
            let ss = List.tail l |> List.map (fun x -> ", " + (fst x) + " " + (snd x)) |> String.Concat
            let s = let hh = (List.head l) in "type " + n + " = { " + (fst hh) + " " + (snd hh) + ss + "}"
            Spacing |> List.append [LineStr(s,"","")] |> List.append ( TypeHeader h d)

    let print (z: defination) =
        match z with
        | Singleton(x) -> printDefination true x
        | Chain(l) ->
            if (List.length l < 2) then raise (Error("Chain length is less than 2"))
            let h = List.append (printDefination true (List.head l)) Spacing 
            let t = List.tail l |> List.map (printDefination false) |> List.map (fun x -> List.append x Spacing) |> List.concat
            List.append h t
    
    let ((_, file, _), header) = FileData in (file, (Definations|> List.map print |> List.concat |> List.append header))




