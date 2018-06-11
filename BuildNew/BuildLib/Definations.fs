module Definations

open Common

type block<'T> = posType<'T nlist>

type lowerIdentifier = posType<string>

type upperIdentifier = posType<string>

type nameSpace = upperIdentifier list

type lowerIdentifierFull = {Name:posType<string>; Namespace:nameSpace}

type upperIdentifierFull = {Name:posType<string>; Namespace:nameSpace}

type basicTypeDef = {Name:either<pos, lowerIdentifier>; GenericArgs: basicTypeDef list} //Everything except functions

type typeDef = {Type:basicTypeDef; Params:basicTypeDef list} // Is String? Is Primitive?

type param = {Type:typeDef; Name:upperIdentifier}

type paramPred = typeDef option

type paramGradual = {Type:typeDef option; Name:upperIdentifier}

type headerDef = {Name:upperIdentifier;  GenericArgs: basicTypeDef list; Params: paramGradual list}

type headerDefNameless = {GenericArgs: basicTypeDef list; Params: paramGradual list}

type primitive (* NOPOS *) = PrimitiveChar of char | PrimitiveInt of int | PrimitiveBool of bool

type op1 (* NOPOS *) = Not | Negative

type op2 (* NOPOS *) = Add (* + *) | Sub (* - *) | Mult (* * *) | Div (* / *) | Gt (* < *) | Lt (* > *) | Ge (* =>, >= *)
                     | Le (* >=, => *) | Eq (* = *) | Neq (* <>, !=, >< *) | Concat (* ++ *) | Join (* H::T *) | Range (* .. *)
                     | In | PipeForward (* >> *) | PipeBackwards (* << *) | And | Or | Is | SeqAll (* :=  *) | SeqAny (* :- *)

type label<'E> = {Name:upperIdentifier; Expr: 'E}

type exprTerm<'E> = { Constructor: lowerIdentifier option ;  Args: 'E nlist } // Can be a term or tuple

type constant = Primitive of primitive | Term of exprTerm<constant>

type expr =
    | ExprSwitch of Pos:pos * Match:unifyExpr * Cases:caseBlock<expr> nlist * Default:expr option
    //| ExprLetIn of Pos:pos * Var:upperIdentifier * Val:expr * Expr:expr // let Var = Val in Expr
    | ExprBinary of posType<op2> * expr * expr
    | ExprUnary of posType<op1> * expr
    | ExprPrimitive of posType<primitive>
    | ExprCall of FuncName:upperIdentifierFull * Args:expr list // Only func call or Var
    | ExprIf of  Pos:pos * Cond:expr * Then:expr * Else:expr
    | ExprTerm of expr exprTerm
    | ExprFor of Pos:pos * Var:upperIdentifier * Init:expr * Cases:loopCase list * Body:block<loopStatement> // for(V = E: cases) { Body }
    | ExprSeqAll of Expr:expr * Body:predStatement
    | ExprSeqAny of Expr:expr * Body:predStatement
    | ExprLambda of Params:paramGradual list * TypeOption:typeDef option * Expr:expr //TODO: Add generic args
    | ExprBlock of Expr:expr * Body:funcBlock
    // TODO: Add local Predicate

and exprLabel = {Name:upperIdentifier; Expr: expr}

and caseBlock<'T> = {Pattern:patternExpr; Condition:expr option; Value:'T}

and loopCase = {Name:upperIdentifier; List:expr} // A in list-expr

and unifyExpr =
    | UnifyExprTerm of unifyExpr exprTerm
    | UnifyExprRegular of expr
    | UnifyExprAnonymous of either<pos,upperIdentifier>
    | UnifyExprBind of upperIdentifier

and patternExpr =
    | PatternExprTerm of patternExpr exprTerm
    | PatternExprRegular of expr
    | PatternExprAnonymous of either<pos,upperIdentifier>

and funcBlock = block<funcStatement>

and loopBlock = block<loopStatement>

and funcStatement =
    | FuncStatementIf of Condition:expr * Then:funcBlock * Else:funcBlock
    | FuncStatementSwitch of Match:expr * Cases:caseBlock<funcBlock> list * Default:funcBlock option
    | FuncStatementDef of defStatement

and predStatement =
    | PredStatementBlock of block<predStatement>
    | PredStatementOr of predStatement * predStatement
    | PredStatementCall of Any:bool * ActionName:upperIdentifierFull * Args:unifyExpr list
    | PredStatementIf of Condition:expr * Then:predStatement * Else:predStatement
    | PredStatementSwitch of Match:unifyExpr * Cases:caseBlock<predStatement> list * Default:predStatement option
    | PredStatementStop
    | PredStatementUnify of expr * expr // The "is" operator
    | PredStatementMember of Var:expr * List:expr // The "in" operator
    | PredStatementAssert of expr //Should not have "in" operator as it is a seperate statement
    | PredStatementDef of defStatement

and predBody = {Args:expr list; block: predStatement}

and actionStatement = // funcStatement, predStatement, actionStatement, loopStatement
    | ActionStatementBlock of block<actionStatement>
    | ActionStatementCall of Any:bool * ActionName:upperIdentifierFull * Args:unifyExpr list
    | ActionStatementIf of Condition:expr * Then:predStatement * Else:predStatement
    | ActionStatementSwitch of Match:unifyExpr * Cases:caseBlock<actionStatement> list * Default:actionStatement option
    | ActionStatementStop
    | ActionStatementError of Message:expr
    | ActionStatementUnify of unifyExpr * patternExpr // The "is" operator
    | ActionStatementFileGenerate of Name:expr * Location:expr * Contents:expr
    | ActionStatementExport of VarName:upperIdentifierFull * Value:expr
    | ActionStatementLog of Message:expr
    | ActionStatementMember of Var:unifyExpr * List:expr // The "in" operator
    | ActionStatementFor of LoopCases:loopCase list * Condition:expr * Body:actionStatement
    | ActionStatementAssert of expr //Should not have "in" operator as it is a seperate statement
    | ActionStatementDef of defStatement

and loopStatement =
    | LoopStatementIf of Condition:expr * Then:loopBlock * Else:loopBlock
    | LoopStatementSwitch of Match:unifyExpr * Cases:caseBlock<loopBlock> list * Default:loopBlock option
    | LoopStatementContinue of expr
    | LoopStatementBreak of expr
    | LoopStatementMember of Var:expr * List:expr // The "in" operator
    | LoopStatementDefFun of defStatement

and defStatement =
    | DefStatementFun of Header:headerDef * TypeDef:typeDef option * Expr:expr
    | DefStatemenPred of Header:headerDef * Params:paramPred list * Body:predBody nlist


type defStatementNameless =
    | DefStatementFun of Header:headerDefNameless * TypeDef:typeDef option * Expr:expr
    | DefStatemenPred of Header:headerDefNameless * Params:paramPred list * Body:predBody nlist

type lowerKey = lowerIdentifier * nameSpace

type upperKey = upperIdentifier * nameSpace

type classDefination = {GenericArgs:basicTypeDef list; Params:param list; Guard:expr option; Definitions:defStatement list}

type lowerDefination =
    | DefClass of classDefination * Interface:lowerIdentifier option
    | DefInterface of GenericArgs:basicTypeDef list * Instances:Map<lowerDefination, classDefination>
    | DefAlias of TypeDef:typeDef

type upperDefination =
    | Def of defStatementNameless
    | DefAction of Header:headerDefNameless option * Body:actionStatement
    | DefConstant of TypeDef:typeDef option * Value:expr
    | DefVar of TypeDef:typeDef

type program0 = {LowerDefinitions:Map<lowerKey, lowerDefination>; UpperDefinitions:Map<upperKey, upperDefination>}

type upperModelDefination =
    | Def of defStatementNameless
    | DefConstant of TypeDef:typeDef option * Value:expr
    | DefVar of TypeDef:typeDef * block<block<predStatement>>

type inContext<'T> = {Item:'T; Context:nameSpace}

type fileGenerator = {Name:expr; Location:expr; Block:block<predStatement>}

type errorGenerator = {Message:expr; Block:block<predStatement>}

//type program = {LowerDefinitions:Map<lowerKey, inContext<lowerDefination>>;
//              UpperModelDefinitions:Map<upperKey, inContext<upperDefination>> }

// Files:inContext<fileGenerator> list;
// Errors:inContext<errorGenerator> list}
