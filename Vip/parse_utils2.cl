% --------------- Class parse_utils: Parse Utilities ---------------

class parse_utils
    open core, lexer

predicates
    parse_identifier0: (token* Input, token* Output, token Result) determ (i,o,o).
    parse_identifier1: (token* Input, token* Output, token Result) determ (i,o,o).
    parse_str: (token* Input, token* Output, token Result) determ (i,o,o).

    match_identifier0: (token* Input, token* Output, string Data) determ (i,o,i).
    match_identifier1: (token* Input, token* Output, string Data) determ (i,o,i).
    match_symbols: (token* Input, token* Output, string Data) determ (i,o,i).

    match_white: (token* Input, token* Output) determ (i,o).

    match_open1: (token* Input, token* Output) determ (i,o).
    match_open2: (token* Input, token* Output) determ (i,o).
    match_open3: (token* Input, token* Output) determ (i,o).

    match_close1: (token* Input, token* Output) determ (i,o).
    match_close2: (token* Input, token* Output) determ (i,o).
    match_close3: (token* Input, token* Output) determ (i,o).

    match_endline: (token* Input, token* Output) determ (i,o).

    trim_white: (token* Input, token* Output) determ (i,o).

    position: (token* Token, string FileName, unsigned LineNum, unsigned Position) determ (i,o,o,o).

    skip: (token*) -> token*.

    parse_error1: (token* Input, string Message, token* Output) determ (i,i,o).
    parse_error2: (token* Input, string Message, token* Output) determ (i,i,o).
    parse_error3: (token* Input, string Message, token* Output) determ (i,i,o).

end class parse_utils


