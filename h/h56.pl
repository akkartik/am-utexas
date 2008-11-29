/* H56 - Any-concept.Examples.Check :
   If any generalization of Concept has many typical examples, all of which
   are also examples of Concept, then add to the conjectures of Concept that
   Concept and the generalization are equivalent, (ie: Concept is really no
   more specialized than the "generalization"), and test this conjecture on
   the boundary examples of the generalization, and see if the generalization
   might be no more specialized than one of its generalizations.           */

h56(Concept) :-
        time(Time),
        Allowed is Time/3,
        clock(Start,_),
        !,
        get(Concept,[defn,name],[Definition]),
        h34a_find_values([genl],Concept,Genls),
    % prevents calls to unknown clauses from suspending am.
%        unknown(Old,fail),
        h56_do_while_time(Start,Allowed,Concept,Definition,Genls)
%,
%        unknown(fail,Old)
.

h56_do_while_time(_,_,_,_,[]).
h56_do_while_time(Start,Allowed,_,_,_) :-
        clock(Start,Elapsed),
        Elapsed > Allowed.
h56_do_while_time(Start,Allowed,Concept,Definition,[Genl|Genls]) :-
        h56_exs_typ(Genl,Typical_examples),
        length(Typical_examples,Length),
        Length > 10,
        getarity(Concept,Arity),
        apply_to_all(Definition,Arity,Typical_examples),
        put(Concept,[conjecs],[equal,Concept,Genl]),
        addtoagenda(check,Concept,[conjecs],200,
        'An untested conjecture has recently been proposed'),
        addtoagenda(check,Genl,[examples,typ],150,
        'A specialization was recently found to be equal'),
        h56_do_while_time(Start,Allowed,Concept,Definition,Genls).
h56_do_while_time(Start,Allowed,Concept,Definition,[Genl|Genls]) :-
        h56_do_while_time(Start,Allowed,Concept,Definition,Genls).

/* H56_exs_typ(Concept,Examples) -
   Examples returns all the typical examples of Concept and all of
   its specializations; (based on procedure exs).               */

h56_exs_typ(Concept,Examples) :-
        ripple(down,Concept,Specializations),
        exs_typ1(Specializations,Exs),
        exs_typ2(Exs,Examples),
        !.

exs_typ1([],[]).
exs_typ1([H|T],Examples) :-
        get(H,[examples,typ],Typ1),
        exs_typ1(T,Typ2),
        append(Typ1,Typ2,Examples).

exs_typ2([],[]).
exs_typ2([H|T],Examples) :-
        ripple(down,H,Typ1),
        exs_typ2(T,Typ2),
        append(Typ1,Typ2,Examples).

/* Apply_to_all(Predicate,Arglists) -
   Is satisfied when Predicate is satisfied by each argument list in Arglists.
*/
apply_to_all(_,_,[]).
apply_to_all(Predicate,Arity,[Arglist|Arglists]) :-
        makeinst(Predicate,Arity,Arglist,Call),
        Call, 
/*      apply(Predicate,[Arglist]),            */
        !,
        apply_to_all(Predicate,Arity,Arglists).

