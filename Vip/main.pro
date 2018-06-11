% --------------- Implement Main ---------------

implement main
    open core

class predicates
    start: (string) determ (i).

clauses
    run():-
        console::init(),
        F = string::trim(mainExe::getCommandLine()),
        if (start(F)) then
            stdio::nl, stdio::write("Exit Success !!!")
        else
            errors::display()
        end if,
        stdio::nl.

    start(F) :-
        lexer::main(F,L),
        errors::check(),
        parser::main(F,L,S),
        errors::check(),
        structure::main(F,S,P),
        errors::check(),
        printer::main(F,P),
        errors::check().

end implement main
