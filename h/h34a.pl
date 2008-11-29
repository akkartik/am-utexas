/* H34a - Any-concept.Examples.Fillin :
   Finds examples of Concept by finding examples of operations whose range
   includes Concept.  Relevent operations are found by collecting the in_ran_of
   values of all specializations of Concept.                                 */

h34a(Concept) :-
        time(Time),
        Allowed is Time/3,
        clock(Start,_),
        get(Concept,[defn,name],[Definition]),
        exs(Concept,Old),
        specs_sf(Concept,Specializations),
        h34a_collect([in_ran_of],Specializations,Operations),
        h34a_collect_examples(Operations,Operation_examples),
        mysetof(Range,
                Operation^ (member(Operation,Operation_examples),
                            lastof(Range,Operation),
                            nonmember(Range,Old)),
                Op_ranges),
        h29_do_while_time_2(Start,Allowed,Definition,Arity,Op_ranges,Examples),
        clock(Start,Elapsed),
        h29_add_and_check_new_values(Concept,[examples,typ],Examples,Elapsed).

/* H34a_collect(Slot,Concepts,Values) -
   Values returns the set containing all the unique values of Slot for each of
   the Concepts.                                                            */

h34a_collect(_,[],[]).
h34a_collect(Slot,[Concept|Concepts],Values) :-
        h34a_find_values(Slot,Concept,Concept_values),
        h34a_collect(Slot,Concepts,More_values),
        append(Concept_values,More_values,All_values),
        removedups(All_values,Values).

/* H34a_find_values(Slot,Concept,Values) -
   Values returns the Slot values for Concept.  When Concept has no value for
   Slot, a task is proposed to fillin the Slot for Concept.                */

h34a_find_values(Slot,Concept,[Val1|Values]) :- 
        get(Concept,Slot,[Val1|Values]),!.
h34a_find_values(Slot,Concept,[]) :-
        addtoagenda(fillin,Concept,Slot,100,
        'Examples might help find operations with a particular range').

/* H43a_collect_examples(Concepts,Examples) -
   Examples returns the set of examples for each concept in Concepts
*/

h34a_collect_examples([],[]).
h34a_collect_examples([Concept|Concepts],Examples) :-
        find_examples(Concept,Concept_examples),
        h34a_collect_examples(Concepts,More_examples),
        append(Concept_examples,More_examples,All_examples),
        removedups(All_examples,Examples).

nonmember(_,[]).
nonmember(X,L) :- \+member(X,L).

