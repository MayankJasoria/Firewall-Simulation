/**
 * Contains the firewall clauses and conditions in prolog format.
 * Examlple of how to edit this prolog file has been provided in comments.
 */
 
default(accept). 
 
/**
 * Adapter clauses format in form of predicates is defined as follows: 
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
accept_adapter(a). 
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
 * Ethernet clauses
 * accept_ether_proto(<proto id>)
 * accept_ether_proto_l({arp|aarp|atalk|ipx|mpls|netbui|pppoe|rarp|sna|xns})
 * accept_ether_vid(<vlan-number>)
 * accept_ether_vid_r(<vlan-number-lower-limit>, <vlan-number-upper-limit>)
 * accept_ether_vid_proto(<vlan-number>, <proto-number>)
 * accept_ether_vid_r_proto(<vlan-number-lower-limit>, <vlan-number-upper-limit>, <proto-id>).
 * accept_ether_vid_proto_l(<vlan-number>, [<proto-id1>,<proto-id2> ... ]).
 * accept_ether_vid_r_proto_l(<vlan-number-lower-limit>, <vlan-number-upper-limit>, [<proto-id1>,<proto-id2> ... ]).
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
 * accept_ip_src_addr('<ipv4-addr>')
 * accept_ip_dst_addr('<ipv4-addr>')
 * accept_ip_addr('<ipv4-addr>')
 * accept_ip_proto('<proto-type>')
 * accept_ip_src_dst_addr('<ipv4-addr>', '<ipv4-addr>')
 * accept_ip_src_dst_addr_proto('<ipv4-addr>', '<ipv4-addr>', <proto-id>)
 * 
 * accept_ip_src_addr_r('<ipv4-addr>', '<ipv4-addr>')
 * accept_ip_dst_addr_r('<ipv4-addr>', '<ipv4-addr>')
 * accept_ip_addr_r('<ipv4-addr>', '<ipv4-addr>')
 * accept_ip_src_dst_addr_r('<ipv4-addr>', '<ipv4-addr>', '<ipv4-addr>', '<ipv4-addr>')
 * accept_ip_src_dst_addr_proto_r('<ipv4-addr>', '<ipv4-addr>', '<ipv4-addr>', '<ipv4-addr>', <proto-id>)
 */	
 
accept_ip_src_addr('172.17.2.11').
accept_ip_dst_addr('b'). 
accept_ip_addr('c').
accept_ip_proto(3).
accept_ip_src_dst_addr('0.0.0.0', '0.0.0.0').
accept_ip_src_dst_addr_proto('0.0.0.0', '0.0.0.0', 1).

accept_ip_src_addr_r('0.0.0.0', '0.0.0.0').
accept_ip_dst_addr_r('0.0.0.0', '0.0.0.0'). 
accept_ip_addr_r('0.0.0.0', '0.0.0.0').
accept_ip_src_dst_addr_r('0.0.0.0', '1.0.0.0', '0.0.0.0', '1.0.0.0').
accept_ip_src_dst_addr_proto_r('10.10.3.9','10.10.3.9', '172.17.2.13', '172.17.2.13', 34).
accept_ip_src_dst_addr_proto_r('10.10.3.9','10.10.3.9', '172.17.2.13', '172.17.2.13', 34).
accept_ip_src_dst_addr_proto_r('10.10.3.9','10.10.3.9', '172.17.2.13', '172.17.2.13', 34).

 