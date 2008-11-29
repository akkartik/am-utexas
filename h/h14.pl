/************************************************************
 *  h14 anycon.suggest
 *      after dealing with C, boost any active con whose
 *      d/r uses C.
 */

h14(C) :-
        get(C,[in_domain_of],D),
        get(C,[in_range_of],R),
        append(D,R,L),
        boost_worth(L,100).

