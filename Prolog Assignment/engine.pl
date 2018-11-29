/**
 * Loading inputs and database module.
 */
:- ensure_loaded('input.pl').
:- use_module('input.pl', [packet/2]).
:- ensure_loaded('database.pl').
:- use_module('database.pl', [accept_adapter/1, accept_adapter_list/2, accept_adapter_l/1]).
:- ensure_loaded('ipv4_helper.pl').
:- use_module('ipv4_helper.pl', [ip_range_compare_str/3]).

rejected(Z) :- nl, write('Packet ID '), write(Z), write(' rejected with a message.').
accepted(Z) :- nl, write('Packet ID '), write(Z), write(' accepted.').
dropped(Z) :- nl, write('Packet ID '), write(Z), write(' dropped!').

/* ### Helper function MOVE IT LATER TO HELPER FILE ### */ 
ip_src_dst_addr_proto_r_check(Q, P, W, V, Z, Src, Dst, Proto) :- 
	 ip_range_compare_str(Q, P, Src), ip_range_compare_str(W, V, Dst), write(Z), write(' '), write(Proto),(Z==Proto).	
	
	

accept(X, Y) :- 
/* Accept adapters */  
	((nth0(0, Y, E),(accept_adapter(E) ; forall(accept_adapter_l(L),accept_adapter_list(L, E)))),
/* Accept ipv4 datagrams */	
	 (nth0(3, Y, Src), nth0(4, Y, Dst), nth0(5, Y, Proto), nl, write(Src),write(' '), write(Dst), write(' '), write(Proto),
		/* And clauses without ranges */ 
       (accept_ip_src_addr(Src); accept_ip_dst_addr(Dst); accept_ip_addr(Src);accept_ip_addr(Dst);accept_ip_proto(Proto); accept_ip_src_dst_addr_proto(Src, Dst, Proto);
	   /* And clauses with ranges */ 
	   forall(accept_ip_src_addr_r(Q,P), ip_range_compare_str(Q, P, Src));
	   forall(accept_ip_dst_addr_r(Q, P), ip_range_compare_str(Q, P, Dst));
	   forall(accept_ip_addr_r(Q, P), ip_range_compare_str(Q, P, Src)); 
	   forall(accept_ip_addr_r(Q, P), ip_range_compare_str(Q, P, Dst));
	   forall(accept_ip_src_dst_addr_proto_r(Q, P, W, V, Z), ip_src_dst_addr_proto_r_check(Q, P, W, V, Z, Src, Dst, Proto))
	   )
	 ),
	accepted(X));dropped(X).
 

/* proc(X,Y) :-  
(nth0(3, Y, Src), nth0(4, Y, Dst), nth0(5, Y, Proto), nl, write(Src),write(' '), write(Dst), write(' '), write(Proto),forall(accept_ip_src_dst_addr_proto_r(Q, P, W, V, Z), ip_src_dst_addr_proto_r_check(Q, P, W, V, Z, Src, Dst, Proto)), write('yes')); write('No').
 */
:- forall(packet(X, Y), accept(X, Y)).
%:- forall(packet(X, Y), reject(X, Y)).

