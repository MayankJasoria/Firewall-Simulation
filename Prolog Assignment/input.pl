/*
 * Database of input packets.
 * Each packet is defined as a list of values 
 * [Adapter, ether-vid, ether-pro, IPv4 src addr, IPV4 dst addr, IPv4 proto, Proto type (TCP/UDP), src TCP/UDP port, dst TCP/UDP port, ICMP code, ICMP msg]
 * NOTE: Packet contains ICMP code and msg only when ipv4 protocol id is 1 (which denotes that ipv4 datagram is ICMP type)
 */
 
% PACKETS WHICH AE EXPECTED TO BE REJECTED
packet(1,  ['A', 500, arp, 	'172.17.2.11', '172.17.2.12',	 2, 	tcp, 	80, 	80]).
packet(2,  ['D', 890, atalk,'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(3,  ['F', 700, ipx, 	'172.17.2.19', '172.17.2.13',	 34, 	tcp,	90,		80]).
packet(4,  ['E', 700, xns, 	'172.17.2.18', '172.56.23.11',	 7, 	tcp, 	1000,   80]).
packet(5,  ['G', 459, aarp, '172.17.2.11', '123.45.45.12',	 2,		tcp, 	80,	    80]).
packet(6,  ['D', 700, mpls, '172.17.2.15', '10.10.1.3',		 9,	    tcp,	8080,   80]).
packet(7,  ['P', 700, netbui, '172.17.2.19', '23.45.45.23',	 3,     tcp, 	90,		80]).
packet(8,  ['B', 269, arp, 	'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(9,  ['L', 700, rarp, '172.17.2.11', '10.23.12.45',	 2,     tcp, 	80,		80]).
packet(10, ['C', 459, aarp, '172.17.2.15', '34.23.67.78',	 9,     tcp, 	8080,	80]).
packet(11, ['H', 500, ipx, 	'172.17.2.19', '19.23.12.45',	 3,     tcp, 	99,	    80]).
packet(12, ['E', 700, xns, 	'172.17.2.18', '10.23.12.45',	 7,     tcp, 	1000,	80]).
packet(13, ['A', 500, ipx, 	'172.17.2.61', '172.17.2.12',	 2, 	tcp, 	81,		8080]).
packet(14, ['C', 500, ipx, 	'172.17.2.55', '172.17.2.11',	 9, 	tcp, 	8081,	8081]).
packet(15, ['H', 500, ipx, 	'10.10.3.91',   '172.17.2.13',	 34, 	tcp, 	91,	    9000]).
packet(16, ['E', 500, ipx, 	'172.17.2.38', '172.56.23.11',	 7, 	tcp, 	99,   90]).
packet(17, ['A', 500, ipx, 	'172.17.2.21', '123.45.45.12',	 2, 	tcp, 	8081,	    78]).
packet(18, ['C', 500, ipx, 	'172.17.2.25', '10.10.1.3',		 9, 	tcp, 	8080,   45]).
packet(19, ['H', 342, ipx, 	'172.17.2.24', '23.45.45.23',	 3, 	tcp, 	90,	    90]).
packet(20, ['E', 500, ipx, 	'172.17.2.23', '87.45.21.23',	 7, 	tcp, 	1000,   1000]).
packet(21, ['A', 500, ipx, 	'172.17.2.11', '45.23.12.45',	 1, 	udp, 	80, 	69,	 0, 0]).
packet(22, ['C', 500, ipx, 	'172.17.2.15', '34.23.67.78',	 1, 	tcp, 	8080, 	69,	 3, 5]).
packet(23, ['H', 500, ipx, 	'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(24, ['E', 500, ipx, 	'172.17.2.18', '45.23.12.45',	 1, 	tcp, 	1000,	69,	 5, 2]).

/*
% PACKETS WHICH ARE EXPECTED TO BE DROPPED
packet(25, [a, 500, arp, 	'172.17.2.11', '172.17.2.12',	 2, 	tcp, 	80, 	80]).
packet(26, [c, 890, atalk, '172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(27, [h, 700, ipx, 	'172.17.2.19', '172.17.2.13',	 34, 	tcp,	90,		80]).
packet(28, [e, 700, xns, 	'172.17.2.18', '172.56.23.11',	 7, 	tcp, 	1000,   80]).
packet(29, [p, 459, aarp, 	'172.17.2.11', '123.45.45.12',	 2,		tcp, 	80,	    80]).
packet(30, [c, 700, mpls, 	'172.17.2.15', '10.10.1.3',		 9,	    tcp,	8080,   80]).
packet(31, [h, 700, netbui,'172.17.2.19', '23.45.45.23',	 3,     tcp, 	90,		80]).
packet(32, [a, 269, arp, 	'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(33, [a, 700, rarp, 	'172.17.2.11', '45.23.12.45',	 2,     tcp, 	80,		80]).
packet(34, [c, 459, aarp, 	'172.17.2.15', '34.23.67.78',	 9,     tcp, 	8080,	80]).
packet(35, [h, 500, ipx, 	'172.17.2.19', '45.23.12.45',	 3,     tcp, 	90,	    80]).
packet(36, [e, 700, xns, 	'172.17.2.18', '45.23.12.45',	 7,     tcp, 	1000,	80]).
packet(37, [a, 500, ipx, 	'172.17.2.61', '172.17.2.12',	 2, 	tcp, 	80,		8080]).
packet(38, [a, 500, ipx, 	'172.17.2.55', '172.17.2.11',	 9, 	tcp, 	8080,	1000]).
packet(39, [a, 500, ipx, 	'10.10.3.91',   '172.17.2.13',	 34, 	tcp, 	90,	    9000]).
packet(40, [a, 500, ipx, 	'172.17.2.38', '172.56.23.11',	 7, 	tcp, 	1000,   90]).
packet(41, [a, 500, ipx, 	'172.17.2.21', '123.45.45.12',	 2, 	tcp, 	80,	    78]).
packet(42, [a, 500, ipx, 	'172.17.2.25', '10.10.1.3',		 9, 	tcp, 	8080,   45]).
packet(43, [a, 500, ipx, 	'172.17.2.24', '23.45.45.23',	 3, 	tcp, 	90,	    90]).
packet(44, [a, 500, ipx, 	'172.17.2.23', '87.45.21.23',	 7, 	tcp, 	1000,   1000]).
packet(45, [a, 500, ipx, 	'172.17.2.11', '45.23.12.45',	 1, 	udp, 	80, 	69,	 0, 0]).
packet(46, [a, 500, ipx, 	'172.17.2.15', '34.23.67.78',	 1, 	tcp, 	8080, 	69,	 3, 5]).
packet(47, [a, 500, ipx, 	'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(48, [e, 500, ipx, 	'172.17.2.18', '45.23.12.45',	 1, 	tcp, 	1000,	69,	 5, 2]).
 
% PACKETS WHICH ARE EXPECTED TO BE ACCEPTED
packet(49, [a, 500, arp, 	'172.17.2.11', '172.17.2.12',	 2, 	tcp, 	80, 	80]).
packet(50, [c, 890, atalk, '172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(51, [h, 700, ipx, 	'172.17.2.19', '172.17.2.13',	 34, 	tcp,	90,		80]).
packet(52, [e, 700, xns, 	'172.17.2.18', '172.56.23.11',	 7, 	tcp, 	1000,   80]).
packet(53, [p, 459, aarp, 	'172.17.2.11', '123.45.45.12',	 2,		tcp, 	80,	    80]).
packet(54, [c, 700, mpls, 	'172.17.2.15', '10.10.1.3',		 9,	    tcp,	8080,   80]).
packet(55, [h, 700, netbui,'172.17.2.19', '23.45.45.23',	 3,     tcp, 	90,		80]).
packet(56, [a, 269, arp, 	'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(57, [a, 700, rarp, 	'172.17.2.11', '45.23.12.45',	 2,     tcp, 	80,		80]).
packet(58, [c, 459, aarp, 	'172.17.2.15', '34.23.67.78',	 9,     tcp, 	8080,	80]).
packet(59, [h, 500, ipx, 	'172.17.2.19', '45.23.12.45',	 3,     tcp, 	90,	    80]).
packet(60, [e, 700, xns, 	'172.17.2.18', '45.23.12.45',	 7,     tcp, 	1000,	80]).
packet(61, [a, 500, ipx, 	'172.17.2.61', '172.17.2.12',	 2, 	tcp, 	80,		8080]).
packet(62, [a, 500, ipx, 	'172.17.2.55', '172.17.2.11',	 9, 	tcp, 	8080,	1000]).
packet(63, [a, 500, ipx, 	'10.10.3.91',   '172.17.2.13',	 34, 	tcp, 	90,	    9000]).
packet(64, [a, 500, ipx, 	'172.17.2.38', '172.56.23.11',	 7, 	tcp, 	1000,   90]).
packet(65, [a, 500, ipx, 	'172.17.2.21', '123.45.45.12',	 2, 	tcp, 	80,	    78]).
packet(66, [a, 500, ipx, 	'172.17.2.25', '10.10.1.3',		 9, 	tcp, 	8080,   45]).
packet(67, [a, 500, ipx, 	'172.17.2.24', '23.45.45.23',	 3, 	tcp, 	90,	    90]).
packet(68, [a, 500, ipx, 	'172.17.2.23', '87.45.21.23',	 7, 	tcp, 	1000,   1000]).
packet(69, [a, 500, ipx, 	'172.17.2.11', '45.23.12.45',	 1, 	udp, 	80, 	69,	 0, 0]).
packet(70, [a, 500, ipx, 	'172.17.2.15', '34.23.67.78',	 1, 	tcp, 	8080, 	69,	 3, 5]).
packet(71, [a, 500, ipx, 	'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(72, [e, 500, ipx, 	'172.17.2.18', '45.23.12.45',	 1, 	tcp, 	1000,	69,	 5, 2]).
*/