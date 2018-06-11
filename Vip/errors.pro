%
%     Project Name : Proton
%     Author: Nikhil Barthwal
%     File Name: main.pro
%     Contents : error class, that handles all error manager
%     Last Modified : Sunday, November 17, 2013
%
% ---------------------------------------------------------------------------

implement errors
    open core

class facts
    count:unsigned :=0.

clauses
    sanity() :- count = 0, !.

    check() :- count = 0, !.
    check() :- count = 1, !, stdio::nl, stdio::write("    1 error is found!"), stdio::nl, stdio::nl, fail.
    check() :- stdio::nl, stdio::write("    ", count, " errors is found!"), stdio::nl, stdio::nl, fail.

    error0(Message) :- count := count+1, stdio::write("Error :"), stdio::write(Message), stdio::nl.
    error1(Message, Filename) :- count := count+1, stdio::write("Error "), stdio::write(Filename), stdio::write(" :"), stdio::write(Message), stdio::nl.
    error2(Message, Filename, Linenum) :- count := count+1, stdio::write("Error "), stdio::write(Filename), stdio::write(" ("), stdio::write(Linenum), stdio::write(") :"), stdio::write(Message), stdio::nl.
    error3(Message, Filename, Linenum, Position) :- count := count+1, stdio::write("Error "), stdio::write(Filename), stdio::write(" ("), stdio::write(Linenum), stdio::write(", "),stdio::write(Position),stdio::write(") :"), stdio::write(Message), stdio::nl.

end implement errors
