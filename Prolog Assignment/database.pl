/**
 * Contains the firewall clauses and conditions in prolog format.
 * Examlple of how to edit this prolog file has been provided in comments.
 */
 
/**
 * Adapter clause format in form of predicates is defined as follows: 
 * accept_adapter(<Adapter-id>) 
 * accept_adapter([list_of_comma_sep_adapter_ids]) 
 * accept_adapter_r(lower_adapter_id, upper_adapter_id)
 */
  
accept_adapter(a).  
 