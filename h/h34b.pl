/* H34b - Any-concept.Examples.Fillin :
   Finds examples of Concept by finding examples of operations whose range
   includes Concept.  All operations which include Concept as a range in 
   their dom_ran slot are considered.                                   */

h34b(Concept) :-
        time(Time),
        Allowed is Time/3,
        clock(Start,_),
        get(Concept,[defn,name],[Definition]),
        exs(Concept,Old),
        specs_sf(activity,Operations),
        mysetof(Range,Op^Op_example^
                (member(Op,Operations),
                 h34b_find_examples(Op,Operation_examples,Concept),
                 member(Op_example,Operation_examples),
                 lastof(Range,Op_example),
                 nonmember(Range,Old)),
                Range_examples),
        getarity(Concept,Arity),
        h29_do_while_time_2(Start,Allowed,Definition,Arity,Range_examples,Examples),
        clock(Start,Elapsed),
        h29_add_and_check_new_values(Concept,[examples,typ],Examples,Elapsed).

/* h34b_find_examples(Op,Operation_examples,Concept) - 
   Operation_examples returns examples of the concept Op, providing that 
   Concept is a valid range value of Op.  When no domain/range values or
   examples are known for Op, tasks will be proposed to fillin Op domain/range
   values or examples (see h34a_find_values and find_examples).             */

h34b_find_examples(Op,Operation_examples,Concept) :-
        h34a_find_values([dom_range],Op,Dom_range),
        mysetof(D_r,
                 (member(D_r,Dom_range),
                  lastof(Concept,D_r)),
                  [H|T]),
        find_examples(Op,Operation_examples),
        !.

