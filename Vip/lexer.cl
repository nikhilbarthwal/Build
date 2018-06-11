% --------------- Class Lexer ---------------

class lexer
    open core

domains
    token =
        identifier0(string Data, string FileName, unsigned Linenum, unsigned Position) ;
        identifier1(string Data, string FileName, unsigned Linenum, unsigned Position) ;
        str(string Data, string FileName, unsigned Linenum, unsigned Position) ;
        num(unsigned Data, string FileName, unsigned Linenum, unsigned Position) ;
        symbols(string Data, string FileName, unsigned Linenum, unsigned Position) ;
        endl(string FileName, unsigned Linenum, unsigned Position) ;
        continue(string FileName, unsigned Linenum, unsigned Position) ;
        comment(string FileName, unsigned Linenum, unsigned Position) ;
        white(string FileName, unsigned Linenum, unsigned Position) ;
        underscore(string FileName, unsigned Linenum, unsigned Position) ;
        open1(string FileName, unsigned Linenum, unsigned Position) ;
        open2(string FileName, unsigned Linenum, unsigned Position) ;
        open3(string FileName, unsigned Linenum, unsigned Position) ;
        close1(string FileName, unsigned Linenum, unsigned Position) ;
        close2(string FileName, unsigned Linenum, unsigned Position) ;
        close3(string FileName, unsigned Linenum, unsigned Position) .

     tokenlist = token*.

predicates
    getPosition: (token, string File, unsigned LineNum, unsigned Position) determ(i,o,o,o).
    main: (string Filename, tokenlist* Output) determ (i,o).

end class lexer

