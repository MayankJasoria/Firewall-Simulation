 # Prolog-Assignment

This is a repository for maintaining all the submission requirements for Prolog Assignment as part of the course Logic in Computer Science.

## Files
- The file engine.pl is the file which should be executed. This is the file which contains the driver code.
- The file database.pl is the file which contains all the predicates which define the database rules for the Firewall. This is where modifications, if any, should be made to configure the Firewall behaviour for different packet clauses.
- The file input.pl is the file in which sample packets have been defined. 
- The file ipv4_helper.pl contains some prefdicates which help to parse the ip addresses, along with other general helper predicates.

### Input format

The input is defined in a predicate `packet/2` (i.e. a predicate called packet having arity 2), whose arguments are:
1. A Packet ID, which is an integer used to identify a packet (helpful for interpreting outputs)
2. A list defining the actual packet
  
  Each packet is defined as a list of values (in order): 
1. Adapter (Any character between 'A' and 'P', both inclusive).
2. Ethernet vlan id (Any integer between 1 to 999).
3. Ethernet protocol alias (Any one of arp, aarp, atalk, ipx, mpls, netbui, pppoe, rarp, sna, xns). Instead of a Hexadecimal value that is normally associated with a protocol, protocol aliases were considered for simplicity.
4. IPv4 source address (of the form 'xxx.xxx.xxx.xxx', where xxx denoted any integer between 1 to 255. For example, '172.17.23.54').
5. IPv4 destination address (of the form 'xxx.xxx.xxx.xxx', where xxx denoted any integer between 1 to 255. For example, '172.17.23.54').
6. IPv4 protocol ID (an integer between *TODO*).
7. IPv4 protocol type (either of tcp or udp. The values tcp and udp have been treated as atoms).
8. Source TCP/UDP port number (an integer between 1 to 65535).
9. Destination TCP/UDP port number (an integer between 1 to 65535).
10. ICMP code (*TODO*)
11. ICMP message (*TODO*)

**NOTE:** Packet contains ICMP code and msg only when ipv4 protocol id is 1 (which denotes that ipv4 datagram is ICMP type)

### Driver predicate

The predicate `decide/2` should be queried to check the behaviour of the firewall for a specific packet, to determine whether a packet is accepted, rejected (in which case a message is given stating the cause for the rejection) or dropped silently. Its arguments (in order) are:
1. The packet ID (as described above).
2. The list defining the actual packet (as described above).

Further, this driver queries three other predicates to determine the conditions:
- `check_reject/2`: This predicate evaluates to true if there is any parameter of the packet which is specified to be rejected.
- `check_drop/2`: This predicate evaluates to true if there is any parameter of the packet which is specified to be dropped.
- `check_accept/2`: This predicate evaluated to true if all the parameters of a packet are as per the specifications for a packet to be accepted.

The evaluation proceeds in the above order, where the decision is based on the first predicate which evaluates to true. Rejection is given the highest preference, followed by drop and finally accept. In case none of these predicates evaluate to true, the default behaviour of the firewall is to silently drop the packet. This ensures that the firewall does not accept any packet whose parameters re not perfectly within the specifications for a packet that is to be accepted, and hence results in a sound system.

### Database predicates

For each of the packet parameters, there are various predicates, for each of the three behaviours - accept, reject and drop. This allows configuration to be done in such a manner that, for each parameter, the behaviour can be defined.

#### Accept Predicates ####

**Adapter clauses:**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`accept_adapter/1` | Adapter ID | Stores an adapter ID which is to be specified as accepted value. |
|`accept_adapter_l/1` | List of Adapter IDs | Stores a list of adapter IDs which are to be specified as accepted values. |
|`accept_adapter_r/2`| Min Adapter ID, Max Adapter ID | Stores a range of adapter IDs (min value, max-value; both inclusive) which are to be specified as accepted values. |

**Ethernet clauses:**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`accept_ether_proto/1`| Protocol ID (alias) | Stores a protocol alias which is to be specified as accepted ethernet protocol.|
|`accept_ether_proto_l/1`| List of Protocol IDs (aliases) | Stores a list of protocol aliases which are to be specified as accepted ethernet protocols.|
|`accept_ether_vid/1`| VLAN ID | Stores an ethernet VLAN ID which is to be specified as an accepted ethernet VLAN ID.|
|`accept_ether_vid_r/2`| Min VLAN ID, Max VLAN ID | Stores a range of VLAN IDs (min value, max-value; both inclusive) which are to be specified as accepted VLAN IDs.|
|`accept_ether_vid_proto/2`| VLAN ID, Protocol ID (alias) | Stores a VLAN ID and an associated protocol ID (alias) which together are to be specified as accepted combination of ethernet VLAN ID and protocol. |
|`accept_ether_vid_proto_l/2`| VLAN ID, List of Protovol IDs (aliases) | Stores a VLAN ID and a list of associated protocol IDS (aliases) which together are to be specified as accepted combination of ethernet VLAN ID and protocols. |
|`accept_ether_vid_r_proto/3`| Min VLAN ID, Max VLAN ID, Protocol ID (alias) | Stores a range of VLAN IDs associated with a single protocol ID (alias) which together are to be specified as accepted combination of ethernet VLAN IDs and protocol. |
|`accept_ether_vid_r_proto_l/3`| Min VLAN ID, Max VLAN ID, List of Protocol IDs (aliases) | Stores a range of VLAN IDs and a list of associated Protocol IDS (aliases) which together are to be specified as accepted combination of ethernet VLAN IDs and protocols. |

**IPv4 Predicates**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`accept_ip_src_addr/1`| Source IPv4 address | Stores a source IPv4 address which is to be specified as accepted IPv4 source address. |
|`accept_ip_src_addr_r/2`| Min Source IPv4 address, Max Source IPv4 address | Stores a range of source IPv4 addresses which are to be specified as accepted IPv4 source addresses. |
|`accept_ip_dst_addr/1`| Destination IPv4 address | Stores a destination IPv4 address which is to be specified as accepted IPv4 destination address. |
|`accept_ip_dst_addr_r/2`| Min Destination IPv4 address, Max Destination IPv4 address | Stores a range of destination IPv4 addresses which are to be specified as accepted IPv4 destination addresses. |
|`accept_ip_addr/1`| IPv4 Address | Stores an IPv4 address which is to be specified as an accepted IPv4 address (both source and destination). |
|`accept_ip_addr_r/2`| Min IPv4 Address, Max IPv4 Address | Stores a range of IPv4 addresses which are to be specified as accepted IPv4 addresses (both source and destination). |
|`accept_ip_proto/1`| IPv4 Protocol ID | Stores an IPv4 Protocol ID which is to be specified as an accepted IPv4 Protocol ID. |
|`accept_ip_src_dst_addr/2`| Source IPv4 address, Destination IPv4 address | Stores a source IPv4 address and a destination IPv4 address which together are to be specified as accepted source and destination IPv4 addresses. |
|`accept_ip_src_dst_addr_proto/3`| Source IPv4 address, Destination IPv4 address, IPv4 Protocol number | Stores a source IPv4 address and a destination IPv4 address along with an IPv4 Protocol ID, which together are to be specified as accepted source and destination IPv4 addresses. |
|`accept_ip_src_dst_addr_r/4`| Min Source IPv4 address, Max source IPv4 address, Min destination IPv4 address, Max destination IPv4 address | Stores a range of source IPv4 addresses and a range of destination IPv4 addresses which together are to be specified as accepted source and destination IPv4 addresses. |
|`accept_ip_src_dst_addr_r/5`| Min Source IPv4 address, Max source IPv4 address, Min destination IPv4 address, Max destination IPv4 address | Stores a range of source IPv4 addresses, a range of destination IPv4 addresses and an IPv4 protocol ID, which together are to be specified as accepted source and destination IPv4 addresses for a specific IPv4 Protocol ID. |

**TCP/UDP Predicates**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`accept_tcp_src_port/1`| Source TCP Port number | Stores a source TCP Port number which is to be specified as an accepted source TCP Port number.|
|`accept_tcp_dst_port/1`| Destination TCP Port number | Stores a destination TCP Port number which is to be specified as an accepted destination TCP Port number.|
|`accept_tcp_src_dst_port/2`| Source TCP Port number, Destination TCP Port number| Stores a source TCP Port number and a destination TCP Port number which together are to be specified as accepted source TCP Port number and destination TCP Port number. |
|`accept_udp_src_port/1`| Source UDP Port number | Stores a source UDP Port number which is to be specified as an accepted source UDP Port number. |
|`accept_udp_dst_port/1`| Destination UDP Port number | Stores a destination UDP Port number which is to be specified as an accepted destination UDP Port number. |
|`accept_srp_src_dst_port/2`| Source UDP Port number, Destination UDP Port number | Stores a source UDP Port number and a destination UDP Port number which together are to be specified as accepted source UDP Port number and destination UDP Port number. |

**ICMP Predicates**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`accept_icmp_type/1`| ICMP protocol type| Stores an ICMP protocol type which is to be specified as an accepted ICMP protocol type.|
|`accept_icmp_code/1`| ICMP message code| Stores an ICMP message code which us to be specified as an accepted ICMP message code.|
|`accept_icmp_type_code/1`| ICMP protocol type, ICMP message code| Stores an ICMP protocol type and an ICMP message code which together are to be specified as accepted ICMP protocol type and ICMP message code.|

#### Reject Predicates ####

**Adapter clauses:**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`reject_adapter/1` | Adapter ID | Stores an adapter ID which is to be specified as rejected value. |
|`reject_adapter_l/1` | List of Adapter IDs | Stores a list of adapter IDs which are to be specified as rejected values. |
|`reject_adapter_r/2`| Min Adapter ID, Max Adapter ID | Stores a range of adapter IDs (min value, max-value; both inclusive) which are to be specified as rejected values. |

**Ethernet clauses:**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`reject_ether_proto/1`| Protocol ID (alias) | Stores a protocol alias which is to be specified as rejected ethernet protocol.|
|`reject_ether_proto_l/1`| List of Protocol IDs (aliases) | Stores a list of protocol aliases which are to be specified as rejected ethernet protocols.|
|`reject_ether_vid/1`| VLAN ID | Stores an ethernet VLAN ID which is to be specified as an rejected ethernet VLAN ID.|
|`reject_ether_vid_r/2`| Min VLAN ID, Max VLAN ID | Stores a range of VLAN IDs (min value, max-value; both inclusive) which are to be specified as rejected VLAN IDs.|
|`reject_ether_vid_proto/2`| VLAN ID, Protocol ID (alias) | Stores a VLAN ID and an associated protocol ID (alias) which together are to be specified as rejected combination of ethernet VLAN ID and protocol. |
|`reject_ether_vid_proto_l/2`| VLAN ID, List of Protovol IDs (aliases) | Stores a VLAN ID and a list of associated protocol IDS (aliases) which together are to be specified as rejected combination of ethernet VLAN ID and protocols. |
|`reject_ether_vid_r_proto/3`| Min VLAN ID, Max VLAN ID, Protocol ID (alias) | Stores a range of VLAN IDs associated with a single protocol ID (alias) which together are to be specified as rejected combination of ethernet VLAN IDs and protocol. |
|`reject_ether_vid_r_proto_l/3`| Min VLAN ID, Max VLAN ID, List of Protocol IDs (aliases) | Stores a range of VLAN IDs and a list of associated Protocol IDS (aliases) which together are to be specified as rejected combination of ethernet VLAN IDs and protocols. |

**IPv4 Predicates**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`reject_ip_src_addr/1`| Source IPv4 address | Stores a source IPv4 address which is to be specified as rejected IPv4 source address. |
|`reject_ip_src_addr_r/2`| Min Source IPv4 address, Max Source IPv4 address | Stores a range of source IPv4 addresses which are to be specified as rejected IPv4 source addresses. |
|`reject_ip_dst_addr/1`| Destination IPv4 address | Stores a destination IPv4 address which is to be specified as rejected IPv4 destination address. |
|`reject_ip_dst_addr_r/2`| Min Destination IPv4 address, Max Destination IPv4 address | Stores a range of destination IPv4 addresses which are to be specified as rejected IPv4 destination addresses. |
|`reject_ip_addr/1`| IPv4 Address | Stores an IPv4 address which is to be specified as an rejected IPv4 address (both source and destination). |
|`reject_ip_addr_r/2`| Min IPv4 Address, Max IPv4 Address | Stores a range of IPv4 addresses which are to be specified as rejected IPv4 addresses (both source and destination). |
|`reject_ip_proto/1`| IPv4 Protocol ID | Stores an IPv4 Protocol ID which is to be specified as an rejected IPv4 Protocol ID. |
|`reject_ip_src_dst_addr/2`| Source IPv4 address, Destination IPv4 address | Stores a source IPv4 address and a destination IPv4 address which together are to be specified as rejected source and destination IPv4 addresses. |
|`reject_ip_src_dst_addr_proto/3`| Source IPv4 address, Destination IPv4 address, IPv4 Protocol number | Stores a source IPv4 address and a destination IPv4 address along with an IPv4 Protocol ID, which together are to be specified as rejected source and destination IPv4 addresses. |
|`reject_ip_src_dst_addr_r/4`| Min Source IPv4 address, Max source IPv4 address, Min destination IPv4 address, Max destination IPv4 address | Stores a range of source IPv4 addresses and a range of destination IPv4 addresses which together are to be specified as rejected source and destination IPv4 addresses. |
|`reject_ip_src_dst_addr_r/5`| Min Source IPv4 address, Max source IPv4 address, Min destination IPv4 address, Max destination IPv4 address | Stores a range of source IPv4 addresses, a range of destination IPv4 addresses and an IPv4 protocol ID, which together are to be specified as rejected source and destination IPv4 addresses for a specific IPv4 Protocol ID. |

**TCP/UDP Predicates**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`reject_tcp_src_port/1`| Source TCP Port number | Stores a source TCP Port number which is to be specified as an rejected source TCP Port number.|
|`reject_tcp_dst_port/1`| Destination TCP Port number | Stores a destination TCP Port number which is to be specified as an rejected destination TCP Port number.|
|`reject_tcp_src_dst_port/2`| Source TCP Port number, Destination TCP Port number| Stores a source TCP Port number and a destination TCP Port number which together are to be specified as rejected source TCP Port number and destination TCP Port number. |
|`reject_udp_src_port/1`| Source UDP Port number | Stores a source UDP Port number which is to be specified as an rejected source UDP Port number. |
|`reject_udp_dst_port/1`| Destination UDP Port number | Stores a destination UDP Port number which is to be specified as an rejected destination UDP Port number. |
|`reject_srp_src_dst_port/2`| Source UDP Port number, Destination UDP Port number | Stores a source UDP Port number and a destination UDP Port number which together are to be specified as rejected source UDP Port number and destination UDP Port number. |

**ICMP Predicates**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`reject_icmp_type/1`| ICMP protocol type| Stores an ICMP protocol type which is to be specified as an rejected ICMP protocol type.|
|`reject_icmp_code/1`| ICMP message code| Stores an ICMP message code which us to be specified as an rejected ICMP message code.|
|`reject_icmp_type_code/1`| ICMP protocol type, ICMP message code| Stores an ICMP protocol type and an ICMP message code which together are to be specified as rejected ICMP protocol type and ICMP message code.|

#### Drop Predicates ####

**Adapter clauses:**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`drop_adapter/1` | Adapter ID | Stores an adapter ID which is to be specified as dropped value. |
|`drop_adapter_l/1` | List of Adapter IDs | Stores a list of adapter IDs which are to be specified as dropped values. |
|`drop_adapter_r/2`| Min Adapter ID, Max Adapter ID | Stores a range of adapter IDs (min value, max-value; both inclusive) which are to be specified as dropped values. |

**Ethernet clauses:**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`drop_ether_proto/1`| Protocol ID (alias) | Stores a protocol alias which is to be specified as dropped ethernet protocol.|
|`drop_ether_proto_l/1`| List of Protocol IDs (aliases) | Stores a list of protocol aliases which are to be specified as dropped ethernet protocols.|
|`drop_ether_vid/1`| VLAN ID | Stores an ethernet VLAN ID which is to be specified as an dropped ethernet VLAN ID.|
|`drop_ether_vid_r/2`| Min VLAN ID, Max VLAN ID | Stores a range of VLAN IDs (min value, max-value; both inclusive) which are to be specified as dropped VLAN IDs.|
|`drop_ether_vid_proto/2`| VLAN ID, Protocol ID (alias) | Stores a VLAN ID and an associated protocol ID (alias) which together are to be specified as dropped combination of ethernet VLAN ID and protocol. |
|`drop_ether_vid_proto_l/2`| VLAN ID, List of Protovol IDs (aliases) | Stores a VLAN ID and a list of associated protocol IDS (aliases) which together are to be specified as dropped combination of ethernet VLAN ID and protocols. |
|`drop_ether_vid_r_proto/3`| Min VLAN ID, Max VLAN ID, Protocol ID (alias) | Stores a range of VLAN IDs associated with a single protocol ID (alias) which together are to be specified as dropped combination of ethernet VLAN IDs and protocol. |
|`drop_ether_vid_r_proto_l/3`| Min VLAN ID, Max VLAN ID, List of Protocol IDs (aliases) | Stores a range of VLAN IDs and a list of associated Protocol IDS (aliases) which together are to be specified as dropped combination of ethernet VLAN IDs and protocols. |

**IPv4 Predicates**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`drop_ip_src_addr/1`| Source IPv4 address | Stores a source IPv4 address which is to be specified as dropped IPv4 source address. |
|`drop_ip_src_addr_r/2`| Min Source IPv4 address, Max Source IPv4 address | Stores a range of source IPv4 addresses which are to be specified as dropped IPv4 source addresses. |
|`drop_ip_dst_addr/1`| Destination IPv4 address | Stores a destination IPv4 address which is to be specified as dropped IPv4 destination address. |
|`drop_ip_dst_addr_r/2`| Min Destination IPv4 address, Max Destination IPv4 address | Stores a range of destination IPv4 addresses which are to be specified as dropped IPv4 destination addresses. |
|`drop_ip_addr/1`| IPv4 Address | Stores an IPv4 address which is to be specified as an dropped IPv4 address (both source and destination). |
|`drop_ip_addr_r/2`| Min IPv4 Address, Max IPv4 Address | Stores a range of IPv4 addresses which are to be specified as dropped IPv4 addresses (both source and destination). |
|`drop_ip_proto/1`| IPv4 Protocol ID | Stores an IPv4 Protocol ID which is to be specified as an dropped IPv4 Protocol ID. |
|`drop_ip_src_dst_addr/2`| Source IPv4 address, Destination IPv4 address | Stores a source IPv4 address and a destination IPv4 address which together are to be specified as dropped source and destination IPv4 addresses. |
|`drop_ip_src_dst_addr_proto/3`| Source IPv4 address, Destination IPv4 address, IPv4 Protocol number | Stores a source IPv4 address and a destination IPv4 address along with an IPv4 Protocol ID, which together are to be specified as dropped source and destination IPv4 addresses. |
|`drop_ip_src_dst_addr_r/4`| Min Source IPv4 address, Max source IPv4 address, Min destination IPv4 address, Max destination IPv4 address | Stores a range of source IPv4 addresses and a range of destination IPv4 addresses which together are to be specified as dropped source and destination IPv4 addresses. |
|`drop_ip_src_dst_addr_r/5`| Min Source IPv4 address, Max source IPv4 address, Min destination IPv4 address, Max destination IPv4 address | Stores a range of source IPv4 addresses, a range of destination IPv4 addresses and an IPv4 protocol ID, which together are to be specified as dropped source and destination IPv4 addresses for a specific IPv4 Protocol ID. |

**TCP/UDP Predicates**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`drop_tcp_src_port/1`| Source TCP Port number | Stores a source TCP Port number which is to be specified as an dropped source TCP Port number.|
|`drop_tcp_dst_port/1`| Destination TCP Port number | Stores a destination TCP Port number which is to be specified as an dropped destination TCP Port number.|
|`drop_tcp_src_dst_port/2`| Source TCP Port number, Destination TCP Port number| Stores a source TCP Port number and a destination TCP Port number which together are to be specified as dropped source TCP Port number and destination TCP Port number. |
|`drop_udp_src_port/1`| Source UDP Port number | Stores a source UDP Port number which is to be specified as an dropped source UDP Port number. |
|`drop_udp_dst_port/1`| Destination UDP Port number | Stores a destination UDP Port number which is to be specified as an dropped destination UDP Port number. |
|`drop_srp_src_dst_port/2`| Source UDP Port number, Destination UDP Port number | Stores a source UDP Port number and a destination UDP Port number which together are to be specified as dropped source UDP Port number and destination UDP Port number. |

**ICMP Predicates**

| Predicate | Parameter(s) | Description |
| :---: | :---: | :--- |
|`drop_icmp_type/1`| ICMP protocol type| Stores an ICMP protocol type which is to be specified as an dropped ICMP protocol type.|
|`drop_icmp_code/1`| ICMP message code| Stores an ICMP message code which us to be specified as an dropped ICMP message code.|
|`drop_icmp_type_code/1`| ICMP protocol type, ICMP message code| Stores an ICMP protocol type and an ICMP message code which together are to be specified as dropped ICMP protocol type and ICMP message code.|