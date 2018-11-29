/*
 * Contains the firewall clauses and conditions in prolog format.
 * Examlple of how to edit this prolog file has been provided in comments.
 */
 
default(accept). 
 
/*
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

% if you do not wish to supply any adapter id, use accept_adapter(x),
accept_adapter_l([a,c,h,e]).
accept_adapter(c). 
accept_adapter_r(a, j).

/*
 * ############# HELPER PREDICATES ##############
 * DO NOT MODIFY/ALTER THIS
 * Predicate takes a list and compares each element with the 
 * assigned predicates for a match
 * ##############################################
 */

/*
 Iterate through list
 */
accept_list([], []).

accept_list([F|R], X) :-
	\+(X=F),
	accept_list(R, X). 

accept_list([F|R], X) :- (X=F). 	
	
/* Parse ranges of values [TODO] */ 

/*
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
 
% in case you do not wish to supply any protocol, use accept_ether_proto(any)
accept_ether_proto_l([arp, aarp, ipx, pppoe, xns]).
accept_ether_proto(arp).
acept_ether_proto(any).
 
% in case you do not wish to define any vlan id, use accept_ether_vid(1000)
accept_ether_vid(500).
accept_ether_vid_r(300,600).

accept_ether_vid_proto(457,arp).
accept_ether_vid_proto(459,aarp).

accept_ether_vid_r_proto(457, 593, arp).
accept_ether_vid_proto_l(700, [arp, xns, pppoe]).
accept_ether_vid_r_proto_l(200, 800, [aarp, ipx, netbui, rarp]).
  
/*
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
 
accept_ip_src_addr('x').
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

/*
 * ##################### DEFAULT VALUES FOR OPTIONAL CLAUSES ######################
 * Please do not modify this section, otherwise the code may break.
 * ################################################################################
 */
  
% adapter id defaults
accept_adapter_l([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]) :- accept_adapter(x).
accept_adapter_r(a, p) :- accept_adapter(x).
  
% Ethernet protocol default
accept_ether_proto_l([arp,aarp,atalk,ipx,mpls,netbui,pppoe,rarp,sna,xns]) :- accept_ether_proto(any).

% ethernet vlan id default
accept_ether_vid_r(1,999) :- accept_ether_vid(1000).