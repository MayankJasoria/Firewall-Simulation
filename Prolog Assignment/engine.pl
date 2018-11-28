/**
 * Loading inputs and database module.
 */
:- ensure_loaded('input.pl').
:- use_module('input.pl', [packet/2]).
:- ensure_loaded('database.pl').
:- use_module('database.pl', [accept_adapter/1, accept_adapter_list/2, accept_adapter_l/1]).

rejected(Z) :- nl, write('Packet ID '), write(Z), write(' rejected with a message.').
accepted(Z) :- nl, write('Packet ID '), write(Z), write(' accepted.').
dropped(Z) :- nl, write('Packet ID '), write(Z), write(' dropped!').

accept(X, Y) :- 
/* Accept adapters */  
	((nth0(0, Y, E),(accept_adapter(E) ; forall(accept_adapter_l(L),accept_adapter_list(L, E)))),
	accepted(X));dropped(X). 
	

 
:- forall(packet(X, Y), accept(X, Y)).
%:- forall(packet(X, Y), reject(X, Y)).

