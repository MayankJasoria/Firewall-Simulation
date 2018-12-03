/*
 * @author Mayank Jasoria <jasoriamayank@gmail.com>
 * @author Shubham Tiwari <sbhtwr170@gmail.com>
 */

/*
 * Database of input packets.
 * Each packet is defined as a list of values 
 * [Adapter, ether-vid, ether-pro, IPv4 src addr, IPV4 dst addr, IPv4 proto, Proto type (TCP/UDP), src TCP/UDP port, dst TCP/UDP port, ICMP code, ICMP msg]
 * NOTE: Packet contains ICMP code and msg only when ipv4 protocol id is 1 (which denotes that ipv4 datagram is ICMP type)
 */
 
 % Packet 1 accepted.
packet(1,  ['A', 500, arp, 	'172.17.2.11', '172.17.2.12',	 2, 	tcp, 	80, 	80]).

% Packet 2 dropped - UDP source port 90 and destination port 69 is configured to be dropped.
% Also, ICMP type 3 and message code 9 is configured to be dropped (However, this is preceeded by previous condition during evaluation)
packet(2,  ['D', 890, atalk,'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).

% Packet 3 accepted.
packet(3,  ['F', 700, ipx, 	'172.17.2.19', '172.17.2.13',	 34, 	tcp,	90,		80]).

% Packet 4 accepted.
packet(4,  ['E', 700, xns, 	'172.17.2.18', '172.56.23.11',	 7, 	tcp, 	1000,   80]).

% Packet 5 accepted.
packet(5,  ['G', 459, aarp, '172.17.2.11', '123.45.45.12',	 2,		tcp, 	80,	    80]).

% Packet 6 rejected - mpls  ethernet protocol is configured to be rejected.
packet(6,  ['D', 700, mpls, '172.17.2.15', '10.10.1.3',		 9,	    tcp,	8080,   80]).

% Packet 7 rejected - netbui ethernet protocol is configured to be rejected.
packet(7,  ['P', 700, netbui, '172.17.2.19', '23.45.45.23',	 3,     tcp, 	90,		80]).

% Packet 8 dropped - UDP source port 90 and destination port 69 is configured to be dropped.
% Also, ICMP type 3 and message code 9 is configured to be dropped (However, this
% is preceeded by previous condition during evaluation).
packet(8,  ['B', 269, arp, 	'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).

% Packet 9 rejected - Adapter ID 'L' is configured to be rejected.
packet(9,  ['L', 700, rarp, '172.17.2.11', '10.23.12.45',	 2,     tcp, 	80,		80]).

% Packet 10 accepted.
packet(10, ['C', 459, aarp, '172.17.2.15', '34.23.67.78',	 9,     tcp, 	8080,	80]).

% Packet 11 dropped - Destination IPv4 address 19.23.12.45 is configured to be
% dropped. Also, TCP source port 99 is configured to be dropped (However, this
% is preceeded by previous condition during evaluation).
packet(11, ['H', 500, ipx, 	'172.17.2.19', '19.23.12.45',	 3,     tcp, 	99,	    80]).

% Packet 12 accepted.
packet(12, ['E', 700, xns, 	'172.17.2.18', '10.23.12.45',	 7,     tcp, 	1000,	80]).

% Packet 13 rejected - TCP source port 81 is configured to be rejected. Also,
% TCP destination port 8080 is configured to be rejected (However, 
% this is preceeded by previous condition during evaluation).
packet(13, ['A', 500, ipx, 	'172.17.2.61', '172.17.2.12',	 2, 	tcp, 	81,		8080]).

% Packet 14 dropped - TCP Destination port 8081 is configured to be dropped.
packet(14, ['C', 500, ipx, 	'172.17.2.55', '172.17.2.11',	 9, 	tcp, 	8081,	8081]).

% Packet 15 dropped - IPv4 Protocol ID 34 is configured to be dropped. Also,
% TCP Destination Port 9000 is configured to be dropped (However, this is 
% preceeded by previous condition during evaluation).
packet(15, ['H', 500, ipx, 	'10.10.3.91',   '172.17.2.13',	 34, 	tcp, 	91,	    9000]).

% Packet 16 dropped - TCP source port 99 is configured to be dropped.
packet(16, ['E', 500, ipx, 	'172.17.2.38', '172.56.23.11',	 7, 	tcp, 	99,   90]).

% Packet 17 accepted.
packet(17, ['A', 500, ipx, 	'172.17.2.21', '123.45.45.12',	 2, 	tcp, 	8081,	    78]).

% Packet 18 accepted.
packet(18, ['C', 500, ipx, 	'172.17.2.25', '10.10.1.3',		 9, 	tcp, 	8080,   45]).

% Packet 19 rejected - Ethernet VLAN ID 342 is configured to be rejected.
packet(19, ['H', 342, ipx, 	'172.17.2.24', '23.45.45.23',	 3, 	tcp, 	90,	    90]).

% Packet 20 rejected - IPv4 destination IP 87.45.21.23 is within the range of IPs 
% 70.0.0.0 to 90.0.0.0 which are configured to be rejected.
packet(20, ['E', 500, ipx, 	'172.17.2.23', '87.45.21.23',	 7, 	tcp, 	1000,   1000]).

% Packet 21 dropped - UDP source port 90 and destination port 69 is configured to be dropped.
packet(21, ['A', 500, ipx, 	'172.17.2.11', '45.23.12.45',	 1, 	udp, 	80, 	69,	 0, 0]).

% Packet 22 rejected - ICMP message code 5 is configured to be rejected. The same is also
% configured to be dropped but rejection check preceeds drop check.
packet(22, ['C', 500, ipx, 	'172.17.2.15', '34.23.67.78',	 1, 	tcp, 	8080, 	69,	 3, 5]).

% Packet 23 dropped - UDP source port 90 and destination port 69 is configured to be dropped.
% Also, ICMP type 3 and message code 9 is configured to be dropped (However, this
% is preceeded by previous condition during evaluation).
packet(23, ['H', 500, ipx, 	'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).

% Packet 24 accepted.
packet(24, ['E', 500, ipx, 	'172.17.2.18', '45.23.12.45',	 1, 	tcp, 	1000,	69,	 5, 2]).

/*
% ADDITIONAL SAMPLE PACKETS
packet(25, ['A', 500, arp, 	'172.17.2.11', '172.17.2.12',	 2, 	tcp, 	80, 	80]).
packet(26, ['C', 890, atalk, '172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(27, ['H', 700, ipx, 	'172.17.2.19', '172.17.2.13',	 34, 	tcp,	90,		80]).
packet(28, ['E', 700, xns, 	'172.17.2.18', '172.56.23.11',	 7, 	tcp, 	1000,   80]).
packet(29, ['P', 459, aarp, 	'172.17.2.11', '123.45.45.12',	 2,		tcp, 	80,	    80]).
packet(30, ['C', 700, mpls, 	'172.17.2.15', '10.10.1.3',		 9,	    tcp,	8080,   80]).
packet(31, ['H', 700, netbui,'172.17.2.19', '23.45.45.23',	 3,     tcp, 	90,		80]).
packet(32, ['A', 269, arp, 	'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(33, ['A', 700, rarp, 	'172.17.2.11', '45.23.12.45',	 2,     tcp, 	80,		80]).
packet(34, ['C', 459, aarp, 	'172.17.2.15', '34.23.67.78',	 9,     tcp, 	8080,	80]).
packet(35, ['H', 500, ipx, 	'172.17.2.19', '45.23.12.45',	 3,     tcp, 	90,	    80]).
packet(36, ['E', 700, xns, 	'172.17.2.18', '45.23.12.45',	 7,     tcp, 	1000,	80]).
packet(37, ['F', 500, ipx, 	'172.17.2.61', '172.17.2.12',	 2, 	tcp, 	80,		8080]).
packet(38, ['A', 500, ipx, 	'172.17.2.55', '172.17.2.11',	 9, 	tcp, 	8080,	1000]).
packet(39, ['D', 500, ipx, 	'10.10.3.91',   '172.17.2.13',	 34, 	tcp, 	90,	    9000]).
packet(40, ['A', 500, ipx, 	'172.17.2.38', '172.56.23.11',	 7, 	tcp, 	1000,   90]).
packet(41, ['K', 500, ipx, 	'172.17.2.21', '123.45.45.12',	 2, 	tcp, 	80,	    78]).
packet(42, ['A', 500, ipx, 	'172.17.2.25', '10.10.1.3',		 9, 	tcp, 	8080,   45]).
packet(43, ['J', 500, ipx, 	'172.17.2.24', '23.45.45.23',	 3, 	tcp, 	90,	    90]).
packet(44, ['P', 500, ipx, 	'172.17.2.23', '87.45.21.23',	 7, 	tcp, 	1000,   1000]).
packet(45, ['A', 500, ipx, 	'172.17.2.11', '45.23.12.45',	 1, 	udp, 	80, 	69,	 0, 0]).
packet(46, ['L', 500, ipx, 	'172.17.2.15', '34.23.67.78',	 1, 	tcp, 	8080, 	69,	 3, 5]).
packet(47, ['H', 500, ipx, 	'172.17.2.19', '45.23.12.45',	 1, 	udp, 	90,		69,	 3, 9]).
packet(48, ['N', 500, ipx, 	'172.17.2.18', '45.23.12.45',	 1, 	tcp, 	1000,	69,	 5, 2]).
*/