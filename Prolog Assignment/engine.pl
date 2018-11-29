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
:- ensure_loaded('ipv4_helper.pl').
:- use_module('ipv4_helper.pl', [ip_range_compare_str/3]).

rejected(Z) :- nl, write('Packet ID '), write(Z), write(' rejected with a message.').
accepted(Z) :- nl, write('Packet ID '), write(Z), write(' accepted.').
dropped(Z) :- nl, write('Packet ID '), write(Z), write(' dropped!').

/* ### Helper function MOVE IT LATER TO HELPER FILE ### */ 
ip_src_dst_addr_proto_r_check(Q, P, W, V, Z, Src, Dst, Proto) :- 
	 ip_range_compare_str(Q, P, Src),
	 ip_range_compare_str(W, V, Dst),
	 write(Z),
	 write(' '),
	 write(Proto),
	 (Z==Proto).	

% ########### DRIVER CLAUSE ###################
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
		/* Accept ipv4 datagrams */	
	 	(
	 		nth0(3, Y, Src), nth0(4, Y, Dst), nth0(5, Y, Proto), nl, write(Src),write(' '), write(Dst), write(' '), write(Proto),
			/* And clauses without ranges */ 
       		(
       			accept_ip_src_addr(Src);
       			accept_ip_dst_addr(Dst);
       			accept_ip_addr(Src);
       			accept_ip_addr(Dst);
       			accept_ip_proto(Proto);
       			accept_ip_src_dst_addr_proto(Src, Dst, Proto);
	   			
	   			/* And clauses with ranges */ 
	   			forall(accept_ip_src_addr_r(Q,P), ip_range_compare_str(Q, P, Src));
	   			forall(accept_ip_dst_addr_r(Q, P), ip_range_compare_str(Q, P, Dst));
			   	forall(accept_ip_addr_r(Q, P), ip_range_compare_str(Q, P, Src)); 
			   	forall(accept_ip_addr_r(Q, P), ip_range_compare_str(Q, P, Dst));
			   	forall(accept_ip_src_dst_addr_proto_r(Q, P, W, V, Z), ip_src_dst_addr_proto_r_check(Q, P, W, V, Z, Src, Dst, Proto))
	   		)
	 	),
		accepted(X)
	);dropped(X).

:- forall(packet(X, Y), decide(X, Y)).