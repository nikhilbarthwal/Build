:- module ex2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is  cc_multi.
:- implementation.

:- import_module solutions.
:- import_module bool.

:- pred text(string::out) is multi.
text("A").
text("B").
text("C").
text("D").
text("E").
text("F").

:- pred print(string::in, bool::out, io::di, io::uo) is det.
print(X, Y, IOState_in, IOState_out) :-	io.write_string(X, IOState_in, IOState_out), ( X = "C" -> Y = no; Y = yes).

%print(IOState_in, IOState_out) :-	IOState_out = IOState_in.


main(IOState_in, IOState_out) :- do_while(text, print, IOState_in, IOState_out).

%	print(IOState_in, IOState_temp),
%	io.write_string("Hello, World!\n", IOState_temp, IOState_out).

% mode do_while(pred(out) is nondet, pred(in, out, di, uo) is det, di, uo)    is cc_multi.
