% --------------- Class Errors: Error Manager ---------------

class errors
    open core

predicates
    check: () determ.
    display: () procedure.
    error0: (string Message) procedure (i).
    error1: (string Message, string Filename) procedure (i,i).
    error2: (string Message, string Filename, unsigned Linenum) procedure (i,i,i).
    error3: (string Message, string Filename, unsigned Linenum, unsigned Position) procedure (i,i,i,i).
end class errors

