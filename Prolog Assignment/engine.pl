/*
 * Loading inputs and database module.
 */
:- ensure_loaded('input.pl').
:- use_module('input.pl', [packet/2]).
:- ensure_loaded('database.pl').

:- use_module('database.pl').
:- ensure_loaded('ipv4_helper.pl').
:- use_module('ipv4_helper.pl', [ip_range_compare_str/3]).

rejected(Z) :- nl, write('Packet ID '), write(Z), write(' rejected with a message.').
accepted(Z, Y) :- nl, write('Packet ID '), write(Z), write(' accepted.'), write(Y).
dropped(Z, Y) :- nl, write('Packet ID '), write(Z), write(' dropped! '), write(Y).

/* ### Helper function MOVE IT LATER TO HELPER FILE ### */ 
ip_src_dst_addr_proto_r_check(Q, P, W, V, Z, Src, Dst, Proto) :- 
	 ip_range_compare_str(Q, P, Src),
	 ip_range_compare_str(W, V, Dst),
	 (Z==Proto).	

% ########### DRIVER CLAUSE ###################
check_accept(X, Y) :-
	(
		/* Accept Adapter */
		(nth0(0, Y, Ad),((accept_adapter_l(L), member(Ad, L)); accept_adapter(Ad))),
		
		/* Accept Ethernet */
		(
			nth0(1, Y, EtVid), nth0(2, Y, EtPr),
			% Checking vor various possible ethernet clauses
			(
				% Checking for accept_ether_vid_r_proto_l
			    forall(accept_ether_vid_r_proto_l(VlanL, VlanH, VPrL), (member(EtPr, VPrL), int_range_compare(VlanL, VlanH, EtVid)));
				
				% Checking for accept_ether_vid_proto_l
				forall(accept_ether_vid_proto_l(EtVid, VPL), member(EtPr, VPL));
				
				% Checking for accept_ether_vid_r_proto
				forall(accept_ether_vid_r_proto(VidL, VidH, EtPr), int_range_compare(VidL, VidH, EtVid));
				
				% Checking for accept_ether_vid_proto
				(accept_ether_vid_proto(EtVid, EtPr));
				
				/*
				  Since none of the above clauses have been satisfied,
				  test for combined satisfaction of any one of the 
				  vlan id clauses and any one of the protocol id clauses
				*/
				% Checking for accept_ether_proto_l
				((accept_ether_proto_l(PL), member(EtPr, PL))), 
				(
					% Checking for accept_ether_vid_r
					% Add required clause here - This takes case of default
					
					% Checking for accept_ether_vid
					accept_ether_vid(EtVid)
				);
				
				% Checking for accept_ether_proto
				(accept_ether_proto(EtPr)),
				(
					% Checking for accept_ether_vid_r
					% Add required clause here - This takes case of default
					
					% Checking for accept_ether_vid
				    accept_ether_vid(EtVid)
				)
			)
		),
		/* Accept ipv4 datagrams */	
	 	(
	 		nth0(3, Y, Src), nth0(4, Y, Dst), nth0(5, Y, Proto), 
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
		/* Accept TCP/UDP ports */
		(
			nth0(6, Y, Proto_tu), nth0(7, Y, Src_port), nth0(8, Y, Dst_port), 
			(
				((Proto_tu == tcp), (accept_tcp_src_port(Src_port); accept_tcp_dst_port(Dst_port); accept_tcp_src_dst_port(Src_port,Dst_port)));
				((Proto_tu == udp), (accept_udp_src_port(Src_port); accept_udp_dst_port(Dst_port); accept_udp_src_dst_port(Src_port,Dst_port)))
			)
				
		),
		/* Accept ICMP packets, if ipv4 proto code is 1 */ 
		(	
			nth0(5, Y, Proto), 
			(\+(Proto == 1);(nth0(9, Y, Type), nth0(10, Y, Code),
			(
				accept_icmp_type(Type);
				accept_icmp_code(Code);
				accept_icmp_type_code(Type, Code)
				
			)))
			
		), 
		accepted(X,Y)
	);dropped(X, Y).

:- forall(packet(X, Y), check_accept(X, Y)).