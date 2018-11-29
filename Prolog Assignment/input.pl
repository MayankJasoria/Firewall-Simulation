/**
 * Database of input packets.
 * Each packet is defined as a list of values 
 * [Adapter, ether-vid, ether-pro, IPv4 src addr, IPV4 dst addr, IPv4 proto, TCP/UDP port, ICMP msg, ICMP code]
 */
 
packet(1, [a, 6, arp, '172.17.2.11', '172.17.2.12' , 2, '80', 'ping']).
packet(2, [a, 5, atalk, '172.17.2.15', '172.17.2.11', 9, '8080', 'ping']).
packet(3, [a, 999, ipx, '10.10.3.9', '172.17.2.13', 34, '90', 'ping']).
packet(4, [a, 23, xns, '172.17.2.18', '172.56.23.11', 7, '1000', 'ping']).
packet(5, [a, 60, aarp, '172.17.2.11', '123.45.45.12', 2, '80', 'ping']).
packet(6, [a, 50, mpls, '172.17.2.15', '10.10.1.3', 9, '8080', 'ping']).
packet(7, [a, 99, netbui, '172.17.2.19', '23.45.45.23', 3, '90', 'ping']).
packet(8, [a, 230, pppoe, '172.17.2.18', '87.45.21.23', 7, '1000', 'ping']).
packet(9, [a, 61, rarp, '172.17.2.11', '45.23.12.45', 2, '80', 'ping']).
packet(10, [a, 51, sna, '172.17.2.15', '34.23.67.78', 9, '8080', 'ping']).
packet(11, [a, 991, ipx, '172.17.2.19', '45.23.12.45', 3, '90', 'ping']).
packet(12, [e, 231, xns, '172.17.2.18', '45.23.12.45', 7, '1000', 'ping']).