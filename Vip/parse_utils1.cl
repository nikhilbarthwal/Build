% --------------- Class parse_utils: Parse Utilities ---------------

class parse_utils
    open core, process

predicates
    match_keyword:    (atom, string) determ (i,i) (i,o)
    match_identifier: (atom, string) determ (i,i) (i,o)
    match_str: (atom, string) determ (i,i) (i,o)
    match_identifierlist: (string, atom) determ (i,i)

    get_position: (atom Input, string FileName, unsigned Lineum, unsigned Position) determ (i,o,o,o)


end class parse_utils



/*
        keyword(string Data, string FileName, unsigned LineNum, unsigned Position) ;
        identifier(string Data, string FileName, unsigned LineNum, unsigned Position) ;
        str(string Data, string FileName, unsigned LineNum, unsigned Position) ;
        num(unsigned Num, string FileName, unsigned LineNum, unsigned Position) ;
        symbols(string Data, string FileName, unsigned LineNum, unsigned Position) ;
        list(atom* List, string FileName, unsigned LineNum, unsigned Position) ;
        datalist(atom* List, string FileName, unsigned LineNum, unsigned Position) .
*/

