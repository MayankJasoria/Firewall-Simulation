/**
 * Reads each member of the list as number and puts them in a list.
 */

parse_num([], []).

parse_num([F|R], [H|T]) :-
	atom_number(F, H),
	parse_num(R,T).

/**
 * Parses the string into a list of size 4 consisting of digits of ip.
 * @param X - ip in string form.
 */
 
ip_to_list(X, L) :-
	split_string(X, ".", "", S),
	parse_num(S, L),
	write(L).

/**
 * Compares the input X with U. All of the list elements of X must be less than or equal to those of U
 * @param U upper range of ip in list form (parsed by ip_to_list/2)
 * @param X the ip [ in list form (parsed by ip_to_list/2) to test.
 */	
 
ip_compare_upper([], []).
ip_compare_upper([B|U], [C|X]) :-
	((B=C);(C<B)),ip_compare_upper(U, X).

/**
 * Compares the input X with U. All of the list elements of X must be greater than or equal to those of L
 * @param L lower range of ip in list form (parsed by ip_to_list/2)
 * @param X the ip [ in list form (parsed by ip_to_list/2) to test.
 */	
 
ip_compare_lower([], []).
ip_compare_lower([A|L], [C|X]) :-	
	((A=C);(C>A)),ip_compare_lower(L, X).
	
/**
 * Compares ips given in list form, to test within the input IP lies in the range.
 * @param L lower range of ip in list form (parsed by ip_to_list/2)
 * @param U upper range of ip in list form (parsed by ip_to_list/2)
 * @param X the ip [ in list form (parsed by ip_to_list/2) to test.
 */	
 
ip_range_compare([A|L], [B|U], [C|X]) :-
	((A=B),((\+(C=A),false) ; ip_range_compare(L, U, X)));
	(
	 ((A<C),(C<B),true);
	 ((C=A), ip_compare_upper(U, X));
	 ((C=B), ip_compare_lower(L, X))
	).

/**
 * Compares ips given in string form, to test within the input IP lies in the range.
 * @param A lower range of ip in string form
 * @param B upper range of ip in string form
 * @param X the ip in string form to test.
 */	
 
ip_range_compare_str(A, B, C) :-
		ip_to_list(A, L),ip_to_list(B, U),ip_to_list(C, X),ip_range_compare(L, U, X).