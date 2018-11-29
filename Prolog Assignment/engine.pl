/*
 * Loading inputs and database module.
 */
:- ensure_loaded('input.pl').
:- use_module('input.pl', [packet/2]).
:- ensure_loaded('database.pl').
:- use_module('database.pl', [accept_adapter/1,
	 accept_list/2,
	 accept_adapter_l/1,
	 accept_ether_vid_proto_l/2,
	 accept_ether_vid_proto/2,
	 accept_ether_proto_l/1,
	 accept_ether_vid/1]).

rejected(Z) :- nl, write('Packet ID '), write(Z), write(' rejected with a message.').
accepted(Z) :- nl, write('Packet ID '), write(Z), write(' accepted.').
dropped(Z) :- nl, write('Packet ID '), write(Z), write(' dropped!').

decide(X, Y) :-
	(
		/* Accept Adapter */
		(nth0(0, Y, Ad),((accept_adapter_l(L), member(Ad, L)); accept_adapter(Ad))),
		
		/* Accept Ethernet */
		(
			nth0(1, Y, EtVid), nth0(2, Y, EtPr), nl, nl,  write(EtPr),  write(' '), write(EtVid),
			% Checking vor various possible ethernet clauses
			(
				% Checking for accept_ether_vid_r_proto_l
				% TODO: add required clause here
				
				% Checking for accept_ether_vid_proto_l
				(nl, write('evpl'), accept_ether_vid_proto_l(EtVid, VPL), member(EtPr, VPL));
				
				% Checking for accept_ether_vid_r_proto
				% TODO: add required clause here
				
				% Checking for accept_ether_vid_proto
				(nl, write('evp'), accept_ether_vid_proto(EtVid, EtPr));
				
				/*
				  Since none of the above clauses have been satisfied,
				  test for combined satisfaction of any one of the 
				  vlan id clauses and any one of the protocol id clauses
				*/
				% Checking for accept_ether_proto_l
				(nl, write('epl'), (accept_ether_proto_l(PL), write(PL), member(EtPr, PL))), 
				(
					% Checking for accept_ether_vid_r
					% Add required clause here - This takes case of default
					
					% Checking for accept_ether_vid
					nl, write('evid'), accept_ether_vid(EtVid)
				);
				
				% Checking for accept_ether_proto
				(nl, write('ep'), accept_ether_proto(EtPr)),
				(
					% Checking for accept_ether_vid_r
					% Add required clause here - This takes case of default
					
					% Checking for accept_ether_vid
					nl, write('evid'), accept_ether_vid(EtVid)
				)
			)
		),
		accepted(X)
	);dropped(X).

:- forall(packet(X, Y), decide(X, Y)).

