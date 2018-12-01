/*
 * Database of input packets.
 * Each packet is defined as a list of values 
 * [Adapter, ether-vid, ether-pro, IPv4 src addr, IPV4 dst addr, IPv4 proto, Proto type (TCP/UDP), src TCP/UDP port, dst TCP/UDP port, ICMP code, ICMP msg]
 * NOTE: Packet contains ICMP code and msg only when ipv4 protocol id is 1 (which denotes that ipv4 datagram is ICMP type)
 */
 

packet(1,  [a, 500, arp, 	'172.17.2.11', '172.17.2.12',	 2, 	tcp, 	80, 	80]).
packet(2,  [c, 890, atalk, 	'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(3,  [h, 700, ipx, 	'172.17.2.19', '172.17.2.13',	 34, 	tcp,	90,		80]).
packet(4,  [e, 700, xns, 	'172.17.2.18', '172.56.23.11',	 7, 	tcp, 	1000,   80]).
packet(5,  [p, 459, aarp, 	'172.17.2.11', '123.45.45.12',	 2,		tcp, 	80,	    80]).
packet(6,  [c, 700, mpls, 	'172.17.2.15', '10.10.1.3',		 9,	    tcp,	8080,   80]).
packet(7,  [h, 700, netbui, '172.17.2.19', '23.45.45.23',	 3,     tcp, 	90,		80]).
packet(8,  [a, 269, arp, 	'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(9,  [a, 700, rarp, 	'172.17.2.11', '45.23.12.45',	 2,     tcp, 	80,		80]).
packet(10, [c, 459, aarp, 	'172.17.2.15', '34.23.67.78',	 9,     tcp, 	8080,	80]).
packet(11, [h, 500, ipx, 	'172.17.2.19', '45.23.12.45',	 3,     tcp, 	90,	    80]).
packet(12, [e, 700, xns, 	'172.17.2.18', '45.23.12.45',	 7,     tcp, 	1000,	80]).

packet(13, [a, 500, ipx, 	'172.17.2.61', '172.17.2.12',	 2, 	tcp, 	80,		8080]).
packet(14, [a, 500, ipx, 	'172.17.2.55', '172.17.2.11',	 9, 	tcp, 	8080,	1000]).
packet(15, [a, 500, ipx, 	'10.10.3.91',   '172.17.2.13',	 34, 	tcp, 	90,	    9000]).
packet(16, [a, 500, ipx, 	'172.17.2.38', '172.56.23.11',	 7, 	tcp, 	1000,   90]).
packet(17, [a, 500, ipx, 	'172.17.2.21', '123.45.45.12',	 2, 	tcp, 	80,	    78]).
packet(18, [a, 500, ipx, 	'172.17.2.25', '10.10.1.3',		 9, 	tcp, 	8080,   45]).
packet(19, [a, 500, ipx, 	'172.17.2.24', '23.45.45.23',	 3, 	tcp, 	90,	    90]).
packet(20, [a, 500, ipx, 	'172.17.2.23', '87.45.21.23',	 7, 	tcp, 	1000,   1000]).
packet(21, [a, 500, ipx, 	'172.17.2.11', '45.23.12.45',	 1, 	udp, 	80, 	69,	 0, 0]).
packet(22, [a, 500, ipx, 	'172.17.2.15', '34.23.67.78',	 1, 	tcp, 	8080, 	69,	 3, 5]).
packet(23, [a, 500, ipx, 	'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(24, [e, 500, ipx, 	'172.17.2.18', '45.23.12.45',	 1, 	tcp, 	1000,	69,	 5, 2]).

