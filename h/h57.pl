/* H57 - Any-concept.Examples.Check :
   If any specialization of Concept has many typical examples, all of which
   are also examples of Concept, then add to the concecture slot of Concept
   that Concept and that specialization are equivalent, (ie: that Concept is
   no more generalized than the "specialization"), and propose tasks to test
   this conjecture on the boundary examples of the specialization and to see
   if the specialization might be equivalent to one of its specializations. */

h57(Concept) :-
        time(Time),
        Allowed is Time/3,
        clock(Start,_),
        h34a_find_values([spec],Concept,Specs),
        h56_exs_typ(Concept,Typicals),
        length(Typicals,Length),
        !,
        Length > 10,
   % prevents calls to unknown clauses from suspending am.
%        unknown(Old,fail),
        h57_do_while_time(Start,Allowed,Concept,Typicals,Specs)
 %  ,     unknown(fail,Old)
  .
h57(Concept).

h57_do_while_time(_,_,_,_,[]).
h57_do_while_time(Start,Allowed,_,_,_) :-
        clock(Start,Elapsed),
        Elapsed > Allowed.
h57_do_while_time(Start,Allowed,Concept,Typicals,[Spec|Specs]) :-
        get(Spec,[defn,name],[Definition]),
        getarity(Concept,Arity),
        apply_to_all(Definition,Arity,Typicals),
        put(Concept,[conjecs],[equal,Concept,Spec]),
        addtoagenda(check,Concept,[conjecs],200,
        'An untested conjecture has recently been proposed.'),
        addtoagenda(check,Spec,[examples,typ],150,
        'A generalization was recently found to be equal.'),
        h57_do_while_time(Start,Allowed,Concept,Typicals,Specs).
h57_do_while_time(Start,Allowed,Concept,Typicals,[Spec|Specs]) :-
        h57_do_while_time(Start,Allowed,Concept,Typicals,Specs).

