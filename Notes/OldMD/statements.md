  Statements and Expressions
==============================

// Pattern to be matched, Condition if present, and block to be executed
caseblock ::= (expr __Pattern__, maybe(expr) __Cond__, block(statement) __Block__)


statement ::=

 - unify(expr __ExprLHS__, expr __ExprRHS__) // Unification: RHS should not have any ?
 - if(condition __Condition__, list(statement) __Then__, maybe(list(statement)) __Else__) // If then Maybe(Else)
 - switch(expr __Expr__, list(caseblock) __Cases__, maybe(caseblock) __DefaultCase__) // Switch with cases & default
 - statement(expr __Expr__) // Either it should be a predicate, i.e a call or it should evalute to boolean
 - stop
 - throw(expr __Error__) // expr should evaluate to string
 - exportFile(expr __FileName__, expr __Contents__) // File and Content should be evaluated to string. In future, concept for folders should be added
 - exportRecord(upperIdentifier __DefName__, list(expr) __Params__) // Params should not have any _ or ?
 - log(expr __Message__) // expr should evaluate to string
 - continue(expr __Expr__)
 - return(expr __Expr__)
 - member(upperIdentifier __Var__, expr __List__)
 - __TODO__: Should have inline function & predicate definitions



pair ::= (maybe(string), X)



expr ::=

 - (|)
 - (&)
 - (!)
 - (>)
 - (>=)
 - (=>)
 - (<)
 - (<=)
 - (=<)
 - (=)
 - (<>)
 - (><)
 - (+)
 - (-)
 - (%)
 - (*)
 - (/)
 - list concat (++)
 - list join (::)
 - unary expr -, !
 - .. operator
 - Var
 - pred call /  func call
 - seq(expr, list(list(statement)))
 - for(upperIdentifier, expr, list(forLoopCase), list(statement))
 - loop(upperIdentifier, expr, list(statement))
 - exprChar(char); // String is list of char
 - exprNum(number);
 - exprBool(bool) - true/false;
 - exprList(list(expr))
 - exprTuple(list(pair(expr))));
 - exprTerm(string, list(expr)).
 - exprIf(pos(expr), pos(expr), pos(expr)); % exprIf(Condition, Then, Else)
 - switch(expr, list(caseblock), maybe(caseblock)) // Switch with cases & default
 - member
