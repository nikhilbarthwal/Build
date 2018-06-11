% --------------- Class process: Convert token list to Structure ---------------

class process
    open core, lexer

domains
    atom =
        keyword(string Data, string FileName, unsigned LineNum, unsigned Position) ;
        identifier(string Data, string FileName, unsigned LineNum, unsigned Position) ;
        str(string Data, string FileName, unsigned LineNum, unsigned Position) ;
        num(unsigned Num, string FileName, unsigned LineNum, unsigned Position) ;
        symbols(string Data, string FileName, unsigned LineNum, unsigned Position) ;
        list(atom* List, string FileName, unsigned LineNum, unsigned Position) ;
        datalist(atom* List, string FileName, unsigned LineNum, unsigned Position) .

predicates
    main: (string FileName, token* Tokens) -> atom.

end class process


