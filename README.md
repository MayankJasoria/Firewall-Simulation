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

NOTE: Packet contains ICMP code and msg only when ipv4 protocol id is 1 (which denotes that ipv4 datagram is ICMP type)

### Driver predicate

The predicate `decide/2` should be queried to check the behaviour of the firewall for a specific packet, to determine whether a packet is accepted, rejected (in which case a message is given stating the cause for the rejection) or dropped silently. Its arguments (in order) are:
1. The packet ID (as described above).
2. The list defining the actual packet (as described above).

Further, this driver queries three other predicates to determine the conditions:
- `check_reject/2`: This predicate evaluates to true if there is any parameter of the packet which is specified to be rejected.
- `check_drop/2`: This predicate evaluates to true if there is any parameter of the packet which is specified to be dropped.
- `check_accept/2`: This predicate evaluated to true if all the parameters of a packet are as per the specifications for a packet to be accepted.

The evaluation proceeds in the above order, where the decision is based on the first predicate which evaluates to true. Rejection is given the highest preference, followed by drop and finally accept. In case none of these predicates evaluate to true, the default behaviour of the firewall is to silently drop the packet. This ensures that the firewall does not accept any packet whose parameters re not perfectly within the specifications for a packet that is to be accepted, and hence results in a sound system.

### Database predicates - Simulating the configuration.ini file

For each of the packet parameters, there are various predicates, for each of the three behaviours - accept, reject and drop. This allows configuration to be done in such a manner that, for each parameter, the behaviour can be defined.

####### Accept clauses
**Adapter clauses:**
| Predicate | Description |
| :---: | :--- |
|`accept_adapter/1` | Stores an adapter ID which is to be specified as accepted value. |
|`accept_adapter_l/1` | Stores a list of adapter IDs which are to be specified as accepted values. |
|`accept_adapter_r/2`| Stores a range of adapter IDs (min value, max-value; both inclusive) which are to be specified as accepted values. |
- Ethernet clauses:
    - `accept_ether_proto/1`: Stores a protocol alias which is to be specified as accepted ethernet protocol.
    - `accept_ether_proto_l/1`: Stores a list of protocol aliases which are to be specified as accepted ethernet protocols.
    - `accept_ether_vid/1`: Stores an ethernet VLAN ID which is to be specified as an accepted ethernet VLAN ID.
    - `accept_ether_vid_l/1`: Stores a list of ethernet VLAN IDS which are to be specified as accepted ethernet VLAN IDs.
    - `accept_ether_vid_r/2`: Stores a range of VLAN IDs (min value, max-value; both inclusive) which are to be specified as accepted VLAN IDs.