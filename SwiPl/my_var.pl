:-use_module(my_list). 

:- ignore(retractall(nikhil_var(_,_))), ignore(retractall(nikhil_link(_,_,_))).

var_del(P) :- ignore(retractall(nikhil_var(P,_))), ignore(retractall(nikhil_link(P,_,_))), ignore(retractall(nikhil_link(_,P,_))), !.

var_set(P,Z) :-  \+ var(Z), P =.. [H1|T1],
	( var_resolve(H1,T1,H2,T2) -> var_get(H2,V), var_del(H2), list_put(Z,T2,V,VV), var_set(H2,VV) ; T1=[], asserta(nikhil_var(H1,Z)) ).

var_link(P,H,T) :- var_resolve(H,T,HH,TT), nikhil_var(HH,V), list_verify(V,TT), var_del(P), asserta(nikhil_link(P,HH,TT)).

var_resolve(H1,T1,H1,T1) :-  nikhil_var(H1,V), list_verify(V,T1), !.
var_resolve(H1,T1,H2,T2) :-  nikhil_link(H1,H2,T), append(T,T1,T2), !.

var_get(P,V) :- P =.. [H|T], var_resolve(H,T,HH,TT), nikhil_var(HH,VV), list_get(VV,TT,V), !.

var_exists(P) :- P =.. [H|T], var_resolve(H,T,_,_), !.

clear :- ignore(retractall(nikhil_var(_,_))), ignore(retractall(nikhil_link(_,_,_))).

% TO BE DONE

:-op(700,fx, +> ).
+> X :- var_exists(X), !.

:-op(700,fx, <+ ).
<+ X :- var_exists(X), !.

:-op(700,fx, ?? ).
?? X :- var_exists(X), !.

:-op(700,fx, ~ ).
~ X :- var_del(X), !.

:-op(700,xfx, <- ).
X <- P :- var_get(P,X), !.

:-op(700,xfx, := ).
P := Z :- \+ var(Z), var_set(P,Z), !.

:-op(700,xfx, += ).
P += Z :- \+ var(Z), var_get(P,V), number(V), ZZ is V + Z, P =..[H|T], var_get(H,X), list_put(ZZ,T,X,Y), var_set(H,Y), !.
        
:-op(700,xfx, -= ).
P -= Z :- \+ var(Z), var_get(P,V), number(V), ZZ is V - Z, P =..[H|T], var_get(H,X), list_put(ZZ,T,X,Y), var_set(H,Y), !.

:-op(700,xfx, *= ).
P *= Z :- \+ var(Z), var_get(P,V), number(V), ZZ is V * Z, P =..[H|T], var_get(H,X), list_put(ZZ,T,X,Y), var_set(H,Y), !.

:-op(700,xfx, /= ).
P /= Z :- \+ var(Z), var_get(P,V), number(V), ZZ is V / Z, P =..[H|T], var_get(H,X), list_put(ZZ,T,X,Y), var_set(H,Y), !.

:-op(900,xfx, <-> ).
P <-> G :- G =.. [H|T], var_link(P,H,T), !.


