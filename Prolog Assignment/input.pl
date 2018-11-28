/**
 * Database of input packets.
 * Each packet is defined as a list of values 
 * [Adapter, IPv4 addr, TCP/UDP port, ICMP msg, ICMP code, ipv4 proto]
 */
 
packet(1, [a, '172.17.2.11', '80', 'ping', '2', '0x86dd']) .
packet(2, [b, '172.17.2.15', '80', 'ping', '9', '0x86dd']) .