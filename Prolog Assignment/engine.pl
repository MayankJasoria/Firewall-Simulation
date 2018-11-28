/**
 * Loading inputs and database module.
 */
:- ensure_loaded('input.pl').
:- use_module('input.pl', [packet/2]).
:- ensure_loaded('database.pl').
:- use_module('database.pl', [accept_adapter/1]).

reject(Z) :- nl, write('Packet ID '), write(Z), write(' rejected with a message.').
accepted(Z) :- nl, write('Packet ID '), write(Z), write(' accepted.').
dropped(Z) :- nl, write('Packet ID '), write(Z), write(' dropped!').


act(X,Y) :- 
	/* check for adapter id */
	

 
:- forall(packet(X, Y), act(X, Y)).

