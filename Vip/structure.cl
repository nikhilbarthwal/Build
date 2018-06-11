% --------------- Class Structure  ---------------

class structure
    open core

domains

    program = p(string Name, lexer::token* ExternList, varBlock*, funcBlock*, condBlock*).

    varBlock = varBlock(lexer::token Name, parser::expr Value).

    funcBlock =
        cFunBlock(lexer::token Name, lexer::token* IdentifierList, parser::cond Condition, statement* Body) ;
        gFunBlock(lexer::token Name, lexer::token* IdentifierList, statement* Body).

    condBlock =
        cCondBlock(lexer::token Name, lexer::token* IdentifierList, parser::cond Condition, statement* Body) ;
        gCondBlock(lexer::token Name, lexer::token* IdentifierList, statement* Body).

    statement =
        unify(parser::expr, parser::expr) ;
        yeild(parser::expr) ;
        check(parser::cond) ; % condition
        map(parser::expr, parser::majorlist, statement* Block) ;
        if_then_else(parser::cond If, statement* Then, statement* Else) ;
        loop(parser::expr , lexer::token V, parser::expr I, parser::majorlist, statement* Block) ;
        forloop(parser::majorlist, statement* Block) ;
        return(parser::expr) ; % only for function
        stop.

predicates
    main: (string Filename, parser::pstatement*, program) determ (i,i,o).

end class structure
