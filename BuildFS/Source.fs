module Source

// TODO Revise blocks

open Common

type block1<'T> = posType<'T list>

type block2<'T> = posType<block1<'T> list>

type identifier = posType<string>

type fullName = {Name:identifier; Namespace:identifier list}

type basicTypeDef = {Name:either<pos, identifier>; GenericArgs: basicTypeDef list} //Everything except functions

type typeDef = {Type:basicTypeDef; Params:basicTypeDef list} //Essentially functions

type param = {Type:typeDef; Name:identifier}

type paramPred = typeDef option

type paramGradual = {Type:typeDef option; Name:identifier}

type headerDef = {Name:identifier;  GenericArgs: basicTypeDef list; Params: paramGradual list}

type primitive (* NOPOS *) = PrimitiveChar of char | PrimitiveInt of int | PrimitiveBool of bool | PrimitiveString of string

type op1 (* NOPOS *) = Not | Negative

type op2 (* NOPOS *) = Add (* + *) | Sub (* - *) | Mult (* * *) | Div (* / *) | Gt (* < *) | Lt (* > *) | Ge (* =>, >= *)
                     | Le (* >=, => *) | Eq (* = *) | Neq (* <>, !=, >< *) | Concat (* ++ *) | Join (* H::T *) | Range (* .. *)
                     | In | PipeForward (* >> *) | PipeBackwards (* << *) | And | Or | Is | SeqAll (* :=  *) | SeqAny (* :- *)

type expr =
    | ExprSwitch of Pos:pos * Match:expr * Cases:caseBlock<expr> list * Default:expr option
    //| ExprLetIn of  Pos:pos * Var:identifier * Val:expr * Expr:expr // let Var = Val in Expr
    | ExprBinary of posType<op2> * expr * expr
    | ExprUnary of posType<op1> * expr
    | ExprPrimitive of posType<primitive>
    | ExprCall of FuncName:fullName * Args:expr list // Only func call or Var
    | ExprIf of  Pos:pos * Cond:expr * Then:expr * Else:expr
    | ExprTerm1 of Constructor:fullName option * Args: expr list // Can be a term or tuple
    | ExprTerm2 of Constructor:fullName option * Args: exprLabel list // Can be a term or tuple
    | ExprAnonymous of Pos:pos * identifier option
    | ExprBind of identifier // X?
    | ExprFor of Pos:pos * Var:identifier * Init:expr * Cases:loopCase list * Body:block2<statement> // for(V = E: cases) { Body }
    | ExprSeqAll of Expr:expr * Body: block2<statement>
    | ExprSeqAny of Expr:expr * Body: block2<statement>
    | ExprLambda of Params:paramGradual list * TypeOption:typeDef option * Expr:expr
    | ExprBlock of Expr:expr * Body: block2<statement>

and exprLabel = {Name:identifier; Expr: expr}

and caseBlock<'T> = {Pattern:expr; Condition:expr option; Value:'T}

and loopCase = {Name:identifier; List:expr} // A in list-expr

and statement = // funcStatement, predStatement, actionStatement, loopStatement
    | StatementBlock of block2<statement>
    | StatementOr of statement * statement
    | StatementCall of Any:bool * ActionName:fullName * Args:expr list
    | StatementIf of Condition:expr * Then:statement * Else:statement
    | StatementSwitch of Match:expr * Cases:caseBlock<block2<statement>> list * Default:block2<statement> option
    | StatementStop
    | StatementError of Message:expr
    | StatementUnify of expr * expr // The "is" operator
    | StatementFileGenerate of Name:expr * Location: expr * Contents: expr //TODO: Later on add location also
    | StatementExport of VarName:fullName * Value:expr
    | StatementLog of Message:expr
    | StatementContinue of expr
    | StatementBreak of expr
    | StatementMember of Var:expr * List:expr // The "in" operator
    | StatementFor of LoopCases:loopCase list * Condition:expr * Body:block2<statement>
    | StatementAssert of expr //Should not have "in" operator as it is a seperate statement
    | StatementDefFun of Header:headerDef * TypeDef:typeDef option * Expr:expr
    | StatementDefPred of Header:headerDef * Params:paramGradual list * Body:block2<statement> //TODO: Revise here

and predBody = {Args:expr list; Body:block1<statement>}

type classDefination = { Name:identifier; GenericArgs:basicTypeDef list; Params:param list; Guard:expr option } // Args can be mempty but params cannot

type defination =
    | DefClass of classDefination
    | DefInterface of Name:identifier * GenericArgs:basicTypeDef list * Instances:classDefination list
    | DefAlias of Name:identifier * TypeDef:typeDef
    | DefVar of Name:identifier * TypeDef:typeDef option * Value:expr option
    | DefFun of Header:headerDef * Params:paramGradual list * TypeDef:typeDef option * Expr:expr
    | DefPred of Header:headerDef * Params:paramPred list * Body:predBody nlist
    | DefAction of Header:headerDef option * Body:block2<statement>

type source = {Namespace:identifier list; Definitions:defination list}
