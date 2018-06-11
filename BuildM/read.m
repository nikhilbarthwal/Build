%------------------------------------------------------------------------------

:- module utils.

:- interface.

:- import_module io.
:- import_module char.
:- import_module maybe.
:- import_module string.

%------------------------------------------------------------------------------

% TODO: Declare the Position variable

:- pred main(io::di, io::uo) is det.

:- pred read_chars(maybe(list(pos(char)))::out, string::in, io::di, io::uo) is det.

%------------------------------------------------------------------------------

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

%------------------------------------------------------------------------------

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (Args = [File| _] ->
        read_chars(Z, File, !IO),
        (Z = yes(C) -> print_chars(C, !IO) ; io.write_string("File read error\n", !IO))
    ;
        io.write_string("No file provided to process\n", !IO)
    ).

:- pred print_chars(list(pos(char))::out, string::in, io::di, io::uo) is det.

print_chars(C, !IO) :- ( C = [H|T] -> print C, print_chars(L, !IO) ; true ) % TODO: Print the C

%------------------------------------------------------------------------------

:- pred read_chars1(list(pos(char)))::out, list(pos(char))::in, list(chars)::in, string::in, int::in, int:in) is det.

read_chars1(L, L0, Chars, File, Line, Pos) :-
    ( Chars = [H|T] ->
        P = {File, Line, Pos},
        L1 = [(P, H) | L0],
        read_chars1(L, L1, T, File, Line, Pos+1)
    ;
        L = L0
    ).

%------------------------------------------------------------------------------

:- pred read_chars0(maybe(list(pos(char)))::out, list(pos(char))::in, string::in, int::in, io::di, io::uo) is det.

read_chars0(L, L0, File, Line, !IO) :-

    io.read_line_as_string(Result, !IO),
    (
        Result = error(_),
        L = no
    ;
        Result = eof,
        L = yes(list.reverse(L0)) % TODO: Check reverse
    ;
        Result = ok(Str),
        string.to_char_list(Str, Chars),
        read_chars1(L1, L0, Chars, File, Line, 1),
        L2 = pos of nl | L1 % TO DO: Proper syntax here
        read_chars0(L, L2, File, Line+1, !IO)
    ).

%------------------------------------------------------------------------------

read_chars(L, File, !IO) :-

    io.see(File, Result, !IO),
    (Result = ok -> read_chars0(L, File, 1, !IO), io.seen(!IO) ; L = no).

