% ****************************************
%    BUILD: Main module for the program
% ****************************************

% 1. Lexer: filename -> token elements
% 2. Parser: token elements -> program
% 3. Analyzer: program -> structure
% 4. Execution: structure -> build/exception

:- module build.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(IO_in, IO_out) :-
    io.command_line_arguments(Args, IO_in, IO1),
    (if (Args = [FileName|_]) then
        io.write_string("Processing file: ", IO1, IO2),
        io.write_string(FileName, IO2, IO3),
        io.nl(IO3, IO4),
        io.set_exit_status(0, IO4, IO_out)
    else
        io.write_string("No file given!", IO1, IO2),
        io.nl(IO2, IO3),
        io.set_exit_status(1, IO3, IO_out)
    ).

