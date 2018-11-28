/**
 * Database of input packets.
 * Each packet is defined as a list of values 
 * [Adapter, ether-vid, ether-pro, IPv4 addr, TCP/UDP port, ICMP msg, ICMP code, ipv4 proto]
 */
 
packet(1, [a, 6, arp, '172.17.2.11', '80', 'ping', '2',]) .
packet(2, [c, 5, atalk, '172.17.2.15', '8080', 'ping', '9']) .
packet(3, [h, 999, ipx, '172.17.2.19', '90', 'ping', '3']) .
packet(4, [e, 23, xns, '172.17.2.18', '1000', 'ping', '7']) .
packet(5, [a, 60, aarp, '172.17.2.11', '80', 'ping', '2',]) .
packet(6, [c, 50, mpls, '172.17.2.15', '8080', 'ping', '9']) .
packet(7, [h, 99, netbui, '172.17.2.19', '90', 'ping', '3']) .
packet(8, [e, 230, pppoe, '172.17.2.18', '1000', 'ping', '7']) .
packet(9, [a, 61, rarp, '172.17.2.11', '80', 'ping', '2',]) .
packet(10, [c, 51, sna, '172.17.2.15', '8080', 'ping', '9']) .
packet(11, [h, 991, ipx, '172.17.2.19', '90', 'ping', '3']) .
packet(12, [e, 231, xns, '172.17.2.18', '1000', 'ping', '7']) .