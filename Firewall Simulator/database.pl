/*
 * @author Mayank Jasoria <jasoriamayank@gmail.com>
 * @author Shubham Tiwari <sbhtwr170@gmail.com>
 */

/*
 * Contains the firewall clauses and conditions in prolog format.
 * Examples of how to edit this prolog file has been provided in comments.
 */ 
 
/*
 * Adapter clauses format in form of predicates is defined as follows: 
 *
 * accept_adapter(<Adapter-id>) 
 * accept_adapter_l([list_of_comma_sep_adapter_ids]) 
 * accept_adapter_r(lower_adapter_id, upper_adapter_id)
 * 
 */

% if you do not wish to supply any adapter id, use accept_adapter(any),
accept_adapter_l(['A','C','H','E']).
accept_adapter('A'). 
accept_adapter_r('A', 'C').

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
% accept_ether_proto(any).
 
% in case you do not wish to define any vlan id, use accept_ether_vid(1000)
accept_ether_vid(500).
accept_ether_vid_r(300,600).

accept_ether_vid_proto(457,arp).
accept_ether_vid_proto(459,aarp).

accept_ether_vid_r_proto(200, 300, arp).
accept_ether_vid_proto_l(700, [arp, xns, pppoe]).
accept_ether_vid_r_proto_l(800, 950, [aarp, ipx, netbui, atalk]).
  
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
 
accept_ip_src_addr('172.17.2.11'). 
accept_ip_src_addr('172.17.2.15').
accept_ip_src_addr('172.17.2.19').
accept_ip_src_addr('172.17.2.18').

accept_ip_dst_addr('b'). 
accept_ip_addr('c').
accept_ip_proto(-1).
accept_ip_src_dst_addr('0.0.0.0', '0.0.0.0').
accept_ip_src_dst_addr_proto('0.0.0.0', '0.0.0.0', 1).

accept_ip_src_addr_r('0.0.0.0', '255.17.56.255').
accept_ip_dst_addr_r('0.0.0.0', '255.255.255.255'). 
accept_ip_addr_r('172.17.0.1', '172.17.60.0').
accept_ip_src_dst_addr_r('0.0.0.0', '1.0.0.0', '0.0.0.0', '1.0.0.0').
accept_ip_src_dst_addr_proto_r('10.10.3.9','10.10.3.9', '172.17.2.13', '172.17.2.13', 34).


/**
 * TCP/UDP Port conditions
 *
 * accept_tcp_src_port(<tcp-udp-port>)
 * accept_tcp_dst_port(<tcp-udp-port>)
 * accept_tcp_src_dst_port(<tcp-udp-port>, <tcp-udp-port>)
 * accept_udp_src_port(<tcp-udp-port>)
 * accept_udp_dst_port(<tcp-udp-port>)
 * accept_udp_src_dst_port(<tcp-udp-port>, <tcp-udp-port>)
 */

accept_tcp_src_port(6969).
accept_tcp_dst_port(6969).
accept_tcp_src_dst_port(80, 69).
accept_udp_src_port(-1).
accept_udp_dst_port(-1).
accept_udp_src_dst_port(80, 69). 
accept_udp_src_dst_port(90, 69). 
 
/**
 * ICMP Port conditions
 *
 * accept_icmp_type(<proto-type>)
 * accept_icmp_code(<message-code>)
 * accept_icmp_type_code(<proto-type>, <message-code>)
 */ 
 
accept_icmp_type(-1).
accept_icmp_code(5).

accept_icmp_type_code(5,2).
accept_icmp_type_code(3,9).
accept_icmp_type_code(0,0).

/* REJECT CLAUSES */ 

/*
 * Contains the firewall clauses and conditions in prolog format.
 * Examlple of how to edit this prolog file has been provided in comments.
 */


/*
 * Adapter clauses format in form of predicates is defined as follows: 
 *
 * reject_adapter(<Adapter-id>) 
 * reject_adapter_l([list_of_comma_sep_adapter_ids]) 
 * reject_adapter_r(lower_adapter_id, upper_adapter_id)
 * 
 */

% if you do not wish to supply any adapter id, use reject_adapter(x),
%reject_adapter_l([c,h,e]).
%reject_adapter(c). 
%reject_adapter_r(a, j).

reject_adapter_l('X').
reject_adapter('L'). 
reject_adapter_r('X', 'X').



/*
 * Ethernet clauses
 * reject_ether_proto(<proto id>)
 * reject_ether_proto_l({arp|aarp|atalk|ipx|mpls|netbui|pppoe|rarp|sna|xns})
 * reject_ether_vid(<vlan-number>)
 * reject_ether_vid_r(<vlan-number-lower-limit>, <vlan-number-upper-limit>)
 * reject_ether_vid_proto(<vlan-number>, <proto-number>)
 * reject_ether_vid_r_proto(<vlan-number-lower-limit>, <vlan-number-upper-limit>, <proto-id>).
 * reject_ether_vid_proto_l(<vlan-number>, [<proto-id1>,<proto-id2> ... ]).
 * reject_ether_vid_r_proto_l(<vlan-number-lower-limit>, <vlan-number-upper-limit>, [<proto-id1>,<proto-id2> ... ]).
 */
 
/*  ### SAMPLE CONFIG ###

reject_ether_proto_l([netbui, mpls]).
reject_ether_proto(netbui).
 

reject_ether_vid(100).
reject_ether_vid_r(100,150).

reject_ether_vid_proto(137,atalk).
reject_ether_vid_proto(129,aarp).

reject_ether_vid_r_proto(200, 300, arp).
reject_ether_vid_proto_l(700, [arp, xns, pppoe]).
reject_ether_vid_r_proto_l(100, 150, [aarp, ipx, netbui, atalk]).

reject_ether_proto_l([netbui, mpls]).
reject_ether_proto(netbui). 

######################## */
 

reject_ether_vid(342).
reject_ether_vid_r(0,0).

reject_ether_vid_proto(0,x).
reject_ether_vid_proto(0,x).

reject_ether_vid_r_proto(0, 0, x).
reject_ether_vid_proto_l(0, [x]).
reject_ether_vid_r_proto_l(0, 0, [x]).
reject_ether_proto_l([netbui, mpls]).
reject_ether_proto(netbui).
  
/*
 * IPv4 clause
 * reject_ip_src_addr('<ipv4-addr>')
 * reject_ip_dst_addr('<ipv4-addr>')
 * reject_ip_addr('<ipv4-addr>')
 * reject_ip_proto('<proto-type>')
 * reject_ip_src_dst_addr('<ipv4-addr>', '<ipv4-addr>')
 * reject_ip_src_dst_addr_proto('<ipv4-addr>', '<ipv4-addr>', <proto-id>)
 * 
 * reject_ip_src_addr_r('<ipv4-addr>', '<ipv4-addr>')
 * reject_ip_dst_addr_r('<ipv4-addr>', '<ipv4-addr>')
 * reject_ip_addr_r('<ipv4-addr>', '<ipv4-addr>')
 * reject_ip_src_dst_addr_r('<ipv4-addr>', '<ipv4-addr>', '<ipv4-addr>', '<ipv4-addr>')
 * reject_ip_src_dst_addr_proto_r('<ipv4-addr>', '<ipv4-addr>', '<ipv4-addr>', '<ipv4-addr>', <proto-id>)
 */	
 
reject_ip_src_addr('0.0.0.0').

reject_ip_dst_addr('0.0.0.0'). 
reject_ip_addr('0.0.0.0').
reject_ip_proto(-1).
reject_ip_src_dst_addr('0.0.0.0', '0.0.0.0').
reject_ip_src_dst_addr_proto('0.0.0.0', '0.0.0.0', 1).

reject_ip_src_addr_r('0.0.0.0', '0.0.0.0').
reject_ip_dst_addr_r('70.0.0.0', '90.0.0.0'). 
reject_ip_addr_r('0.0.0.0', '0.0.0.0').
reject_ip_src_dst_addr_r('0.0.0.0', '1.0.0.0', '0.0.0.0', '1.0.0.0').
reject_ip_src_dst_addr_proto_r('0.0.0.0','0.0.0.0', '0.0.0.0', '0.0.0.0', 34).


/**
 * TCP/UDP Port conditions
 *
 * reject_tcp_src_port(<tcp-udp-port>)
 * reject_tcp_dst_port(<tcp-udp-port>)
 * reject_tcp_src_dst_port(<tcp-udp-port>, <tcp-udp-port>)
 * reject_udp_src_port(<tcp-udp-port>)
 * reject_udp_dst_port(<tcp-udp-port>)
 * reject_udp_src_dst_port(<tcp-udp-port>, <tcp-udp-port>)
 */

 
reject_tcp_src_port(81).
reject_tcp_dst_port(8080).
reject_tcp_src_dst_port(-1, -1).
reject_udp_src_port(-1).
reject_udp_dst_port(-1).
reject_udp_src_dst_port(-1, -1). 
reject_udp_src_dst_port(-1, -1). 
 
/**
 * ICMP Port conditions
 *
 * reject_icmp_type(<proto-type>)
 * reject_icmp_code(<message-code>)
 * reject_icmp_type_code(<proto-type>, <message-code>)
 */ 
 
reject_icmp_type(-1).
reject_icmp_code(5).

reject_icmp_type_code(-1,-1).
reject_icmp_type_code(-1,-1).
reject_icmp_type_code(-1,-1). 

/*
 * Contains the firewall clauses and conditions in prolog format.
 * Examlple of how to edit this prolog file has been provided in comments.
 */
 
/*
 * Adapter clauses format in form of predicates is defined as follows: 
 *
 * drop_adapter(<Adapter-id>) 
 * drop_adapter_l([list_of_comma_sep_adapter_ids]) 
 * drop_adapter_r(lower_adapter_id, upper_adapter_id)
 * 
 */

% if you do not wish to supply any adapter id, use drop_adapter(x),

/* ### SAMPLE CONFIG ###

drop_adapter_l(['C','H', 'E']).
drop_adapter('C'). 
drop_adapter_r('A', 'J').

#########################*/


drop_adapter_l('X').
drop_adapter('X'). 
drop_adapter_r('X', 'X').



/*
 * Ethernet clauses
 * drop_ether_proto(<proto id>)
 * drop_ether_proto_l({arp|aarp|atalk|ipx|mpls|netbui|pppoe|rarp|sna|xns})
 * drop_ether_vid(<vlan-number>)
 * drop_ether_vid_r(<vlan-number-lower-limit>, <vlan-number-upper-limit>)
 * drop_ether_vid_proto(<vlan-number>, <proto-number>)
 * drop_ether_vid_r_proto(<vlan-number-lower-limit>, <vlan-number-upper-limit>, <proto-id>).
 * drop_ether_vid_proto_l(<vlan-number>, [<proto-id1>,<proto-id2> ... ]).
 * drop_ether_vid_r_proto_l(<vlan-number-lower-limit>, <vlan-number-upper-limit>, [<proto-id1>,<proto-id2> ... ]).
 */
 
 

drop_ether_vid(0).
drop_ether_vid_r(0,0).

drop_ether_vid_proto(0,x).
drop_ether_vid_proto(0,x).

drop_ether_vid_r_proto(0, 0, x).
drop_ether_vid_proto_l(0, [x]).
drop_ether_vid_r_proto_l(0, 0, [x]).
drop_ether_proto_l([netbui, mpls]).
drop_ether_proto(netbui).
  
/*
 * IPv4 clause
 * drop_ip_src_addr('<ipv4-addr>')
 * drop_ip_dst_addr('<ipv4-addr>')
 * drop_ip_addr('<ipv4-addr>')
 * drop_ip_proto('<proto-type>')
 * drop_ip_src_dst_addr('<ipv4-addr>', '<ipv4-addr>')
 * drop_ip_src_dst_addr_proto('<ipv4-addr>', '<ipv4-addr>', <proto-id>)
 * 
 * drop_ip_src_addr_r('<ipv4-addr>', '<ipv4-addr>')
 * drop_ip_dst_addr_r('<ipv4-addr>', '<ipv4-addr>')
 * drop_ip_addr_r('<ipv4-addr>', '<ipv4-addr>')
 * drop_ip_src_dst_addr_r('<ipv4-addr>', '<ipv4-addr>', '<ipv4-addr>', '<ipv4-addr>')
 * drop_ip_src_dst_addr_proto_r('<ipv4-addr>', '<ipv4-addr>', '<ipv4-addr>', '<ipv4-addr>', <proto-id>)
 */	
 
 
drop_ip_src_addr('172.17.2.111'). 

drop_ip_dst_addr('19.23.12.45'). 
drop_ip_addr('10.23.12.44').
drop_ip_proto(34).
drop_ip_src_dst_addr('0.0.0.0', '0.0.0.0').
drop_ip_src_dst_addr_proto('0.0.0.0', '0.0.0.0', 1).

drop_ip_src_addr_r('0.0.0.0', '0.0.0.0').
drop_ip_dst_addr_r('0.0.0.0', '0.0.0.0'). 
drop_ip_addr_r('0.0.0.0', '0.0.0.0').
drop_ip_src_dst_addr_r('0.0.0.0', '0.0.0.0', '0.0.0.0', '0.0.0.0').
drop_ip_src_dst_addr_proto_r('0.0.0.0','0.0.0.0', '0.0.0.0', '0.0.0.0', 34).



/**
 * TCP/UDP Port conditions
 *
 * drop_tcp_src_port(<tcp-udp-port>)
 * drop_tcp_dst_port(<tcp-udp-port>)
 * drop_tcp_src_dst_port(<tcp-udp-port>, <tcp-udp-port>)
 * drop_udp_src_port(<tcp-udp-port>)
 * drop_udp_dst_port(<tcp-udp-port>)
 * drop_udp_src_dst_port(<tcp-udp-port>, <tcp-udp-port>)
 */

 
drop_tcp_src_port(99).
drop_tcp_src_port(89).
drop_tcp_dst_port(8081).
drop_tcp_dst_port(9000).
drop_tcp_src_dst_port(80, 69).
drop_udp_src_port(-1).
drop_udp_dst_port(-1).
drop_udp_src_dst_port(80, 69). 
drop_udp_src_dst_port(90, 69). 
 
/**
 * ICMP Port conditions
 *
 * drop_icmp_type(<proto-type>)
 * drop_icmp_code(<message-code>)
 * drop_icmp_type_code(<proto-type>, <message-code>)
 */ 
 
drop_icmp_type(-1).
drop_icmp_code(5).

drop_icmp_type_code(3,9).
drop_icmp_type_code(0,0). 

/*
 * ##################### DEFAULT VALUES FOR OPTIONAL CLAUSES ######################
 * Please do not modify this section, otherwise the code may break.
 * ################################################################################
 */
  
% adapter id defaults
accept_adapter_l(['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P']) :- accept_adapter(any).
accept_adapter_r('A', 'P') :- accept_adapter(x).
  
% Ethernet protocol default
accept_ether_proto_l([arp,aarp,atalk,ipx,mpls,netbui,pppoe,rarp,sna,xns]) :- accept_ether_proto(any).

% ethernet vlan id default
accept_ether_vid_r(1,999) :- accept_ether_vid(1000).


