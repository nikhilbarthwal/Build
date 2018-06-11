% ***********************************************************
%    DEFINATIONS.FS: All the definations for the program
% ***********************************************************


% TO DO: adding String format to expr

% 1. Complete Definations (with exceptions)
% 2. Design Transformer(Old Program->New Program, ErrorList)
% 3. Executionaer
% 4. Builder
% 5. Main
% 6.

:- module definations.

:- interface.

:- import_module io, int, list, string, bool, char.

:- pred main(io::di, io::uo) is det.

:- type counter == int. % Unsigned Number

:- type number == int.  % Signed Number

:- type position == { string, counter, counter }. % (filename, linepos, counter)

:- type pos(T) ---> position(val::T, position).

:- type block(T) == list(pos(T)).

:- type operator1 ---> opNegate ; opNot.

:- type operator2 ---> opAdd ; opSub ; opMult ; opDiv ; opAppend ; opNeq ; opGt ;
                       opGe ; opLt ; opLe ; opAnd ; opOr ; opEq ; opIn.

%:- type identifier ---> this(pos(string)) ; that(pos(string), pos(string)).
:- type identifier == pos(string).

:- type data.
:- type expr.
:- type pattern.
:- type pair.
:- type predType.
:- type ruleType.
:- type statement(_).
:- type typeDef.

:- type data --->
    dataChar(char);
    dataNum(number);
    dataBool(bool);
    dataFunction(list(string), expr);
    dataOperator1(operator1);
    dataOperator2(operator2);
    dataTuple(list(data));
    dataTerm(string, list(data)).

:- type expr --->
    exprIf(pos(expr), pos(expr), pos(expr)); % exprIf(Condition, Then, Else)
    exprData(data); % Data
    exprVar(identifier); % Variable
    exprCall(identifier, block(expr)); %  Func/ Pred call with all vars bound
    exprOp1(operator1, pos(expr));
    exprOp2(operator2, pos(expr), pos(expr));
    exprTerm(pos(string), block(expr)); % Term expression
    exprLet(list(pair), pos(expr));
    exprSeq(block(statement(pred)), pos(expr));
    exprEval(block(expr), pos(expr));
    exprSwitch(pos(expr), block(switchCaseExpr), pos(expr)). % Expr, Case list, Default Case

:- type pair == { pos(string),  pos(expr) }.

:- type switchCaseExpr == { pattern, pos(expr), pos(expr) }.

:- type pattern --->
    patternExpr(pos(expr));
    patternIgnore(position);
    patternUnify(pos(string));
    patternTerm(pos(string), block(pattern)).

:- type statement(T) --->
    statementAssert(pos(expr));
    statementStop;
    statementContinue;
    statementUnify(pos(pattern), pos(pattern));
    statementIf(pos(expr), block(statement(T)));
    statementIfElse(pos(expr), block(statement(T)), block(statement(T)));
    statementCall(pos(string), block(pattern));
    statementSwitch(pos(pattern), block(switchCase(T)), block(statement(T)));% Expr, Case list, Default Case
    statementMember(pos(pattern), pos(pattern));
    statementCustom(T).

:- type switchCase(T) == { pattern, pos(expr), block(statement(T)) }.

:- type predType ---> predStatementOr(list(block(statement(predType)))).

:- type ruleType --->
    ruleStatementAnd(list(block(statement(predType))));
    ruleStatementExport(identifier, block(string));
    ruleStatementDesignError(pos(expr));
    ruleStatementFileGenerate(pos(expr), pos(expr)).

:- type param --->
    paramTyped(pos(typeDef), pos(string));
    paramUntyped(pos(string)).

:- type coreDef --->
    defFunc(pos(string), block(param), pos(expr));
    defPred(pos(string), block(param), block(statement(predType)));
    defConst(pos(param), pos(data)).

:- type typeDef --->
    typeTuple(block(typeDef));
    typeCustom(pos(string), block(typeDef)).

:- type entity --->
    entityRule(block(statement(ruleType)));
    entityNamedRule(pos(string), block(param), block(statement(ruleType)));
    entityRuleHead(pos(string), block(typeDef)); % Block of Params without names
    entityCore(coreDef).

%:- type moduleDef == { pos(string), block(parse) }.

:- type program == list(moduleDef)


%:- type defKey == { string, int }.

:- implementation.

main(!IO) :- io.write_string("Hello, ", !IO), io.nl(!IO).

% :- type calc_info == map(string, int).


% type Transform = (Identifier list) * (Pred Statement block)

% type Transforms = Transform list

% type ProgramError = Position * pExpr * (Pred Statement block)

% type ProgramFile = Position * pExpr * pExpr * (Pred Statement block)

% type ProgramMap = Map<(string*int), Transforms Def>

% type Program =  ProgramMap * (ProgramError list) * (ProgramFile list)

% type TokenGroup = TokenGroup1 | TokenGroup2 | TokenGroup3

:-type Element
%   | ElementList of TokenGroup * Elements
%   | ElementIdentifier0 of string
%   | ElementIdentifier1 of string
%   | ElementString of string
%   | ElementAnonymous
%   | ElementUnify of string
%   | ElementNumber of integer
%   | ElementSymbols of string

% and Elements = (Position * Element) list

% type State = Map<string, Data>

% type States = State list option

% type sourceName = string list
% type sourceContent = string list
% type source = sourceName * sourceContent
% type build = source list







