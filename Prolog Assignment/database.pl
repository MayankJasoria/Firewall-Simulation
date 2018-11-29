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
accept_adapter(u). 
accept_adapter(y).

/**
 * ############# HELPER PREDICATE ##############
 * DO NOT MODIFY/ALTER THIS
 * Predicate takes a list and compares each element with the 
 * assigned predicates for a match
 * ##############################################
 */

accept_list([], []).

accept_adapter_list([F|R], X) :-
	\+(X=F),
	accept_adapter_list(R, X). 

accept_adapter_list([F|R], X) :- (X=F). 	
	
/* Parse ranges of values [TODO] */ 


/**
 * Ethernet clause
 * accept_ether_proto(<proto id>)
 * accept_ether_proto_l({arp|aarp|atalk|ipx|mpls|netbui|pppoe|rarp|sna|xns})
 * accept_ether_vid(<vlan-number>)
 * accept_ether_vid_proto(<vlan-number>, <proto-number>)
 */
 
accept_ether_proto_l([arp, aarp, ipx, pppoe, xns]).
accept_ether_proto(arp).
accept_ether_proto(ipx).
 
accept_ether_vid(354).
accept_ether_vid(355).
accept_ether_vid(356).
accept_ether_vid(357).
accept_ether_vid_r(300,600).

accept_ether_vid_proto(457,arp).
accept_ether_vid_proto(459,aarp).

accept_ether_vid_r_proto(457, 593, arp).
accept_ether_vid_proto_l(700, [arp, xns, pppoe]).
accept_ether_vid_r_proto_l(200, 800, [aarp, ipx, netbui, rarp]).
  
/**
 * IPv4 clause
 * accept_ip_src_addr(<ipv4-addr>)
 * accept_ip_dst_addr(<ipv4-addr>)
 * accept_ip_addr(<ipv4-addr>)
 * accept_ip_proto(<proto-type>)
 * accept_ip_src_dst_addr(<ipv4-addr>, <ipv4-addr>)
 * accept_ip_src_dst_proto_addr(<ipv4-addr>, <ipv4-addr>, <proto-id>)
 */	