% --------------- Class Execute  ---------------

class execute
    open core, structure

predicates
    main: (string FileName, string Feature, lexer::token* Externs, varBlock* Vars, funcBlock* Funcs, condBlock* Conds) procedure (i,i,i,i,i,i).

end class execute

