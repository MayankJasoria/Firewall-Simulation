/**
 * Contains the firewall clauses and conditions in prolog format.
 * Examlple of how to edit this prolog file has been provided in comments.
 */
 
default(accept). 
 
/**
 * Adapter clause format in form of predicates is defined as follows: 
 *
 * accept_adapter(<Adapter-id>) 
 * accept_adapter_l([list_of_comma_sep_adapter_ids]) 
 * accept_adapter_r(lower_adapter_id, upper_adapter_id)
 *
 * reject_adapter(<Adapter-id>) 
 * reject_adapter_l([list_of_comma_sep_adapter_ids]) 
 * reject_adapter_r(lower_adapter_id, upper_adapter_id)
 *
 * drop_adapter(<Adapter-id>) 
 * drop_adapter_l([list_of_comma_sep_adapter_ids]) 
 * drop_adapter_r(lower_adapter_id, upper_adapter_id)
 * 
 */
  
  
accept_adapter_l([a,c,h,e]).
accept_adapter(x). 
accept_adapter(z). 
accept_adapter(y).  
accept_adapter(u).

/* Recurse through the list to check if any matches. Do not alter this */

accept_adapter_list([], []).

accept_adapter_list([F|R], X) :-
	\+(X=F),
	accept_adapter_list(R, X). 

accept_adapter_list([F|R], X) :- (X=F). 	
	
/* Parse ranges of values [TODO] */ 


/**
 * Ethernet clause
 * accept_ether_proto(<proto id>)
 * accept_ether_proto({arp|aarp|atalk|ipx|mpls|netbui|pppoe|rarp|sna|xns})
 * accept_ether_vid(<vlan-number>)
 * accept_ether_vid_proto(<vlan-number>, <proto-number>)
 * 
 */
 
/**
 * IPv4 clause
 * accept_ip_src_addr(<ipv4-addr>)
 * accept_ip_dst_addr(<ipv4-addr>)
 * accept_ip_addr(<ipv4-addr>)
 * accept_ip_proto(<proto-type>)
 * accept_ip_src_dst_addr(<ipv4-addr>, <ipv4-addr>)
 * accept_ip_src_dst_proto_addr(<ipv4-addr>, <ipv4-addr>, <proto-id>)
 */
	
	