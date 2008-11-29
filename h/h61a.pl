/* H61a - Any-concept.Examples,typical.Check :

   *** Encodes H59, H61, and H62 for typical examples

   Ensure typical examples satisfy the Concept's definition, then see if they 
   also satisfy any specialization definition.  If so, then remove them from 
   the Concept's typical examples list and add them to those specializations'
   typical examples lists.  When any example fails to satisfy the Concept's 
   definition then remove it from the Concept's examples list and see if it
   satisfies any generalization of the concept.  If so, then add it to the 
   boundary examples lists of those generalizations.                       */
h61a(Concept) :-
        time(Time),
        Allowed is Time/3,
        clock(Start,_),
        !,
        get(Concept,[defn,name],[Defn]),
        h34a_find_values([spec],Concept,Specs),
        h34a_find_values([genl],Concept,Genls),
        h34a_find_values([examples,typ],Concept,Examples),
        h61a_do_while_time(Start,Allowed,Concept,Defn,Examples,Specs,Genls).

/* h61a_do_while_time(Start,Allowed,Concept,Defn,Examples,Specs,Genls) -
   While time remains, h61a_do_while_time processes each example of Concept
   in Examples, ensuring that it is appropriately included as an example of
   Concept and should not be moved to the examples list of some specialization
   or generalization of Concept.                                            */

h61a_do_while_time(_,_,_,_,[],_,_).
h61a_do_while_time(Start,Allowed,_,_,_,_,_) :-
        clock(Start,Elapsed),
        Elapsed > Allowed.
h61a_do_while_time(Start,Allowed,Concept,Defn,[Example|Examples],Specs,Genls):-
        getarity(Concept,Arity),
        makeinst(Defn,Arity,Example,Call),
        Call,
/*      apply(Defn,[Example]),         */
        h61a_put_if_example(Concept,Example,Specs,[examples,typ]),
        h61a_do_while_time(Start,Allowed,Concept,Defn,Examples,Specs,Genls).
h61a_do_while_time(Start,Allowed,Concept,Defn,[Example|Examples],Specs,Genls):-
        fremove(Concept,[examples,typ],Example),
        h61a_put_if_example(Concept,Example,Genls,[examples,bnd]),
        h61a_do_while_time(Start,Allowed,Concept,Defn,Examples,Specs,Genls).

/* h61a_put_if_example(Concept,Example,Specs,Slot) -
   Specs is either the generalizations or the specializations of Concept.
   Example is a typical example of Concept, and is tested to see if it
   satisfies any of the concepts in Specs.  When any such member of Specs
   is found, Example is removed from the examples list of Concept and is 
   added to the Slot examples list of that concept from Specs.  Slot is 
   assumed to be [examples,typ] when Specs are Concept's specializations;
   Slot is [examples,bnd] when Specs are Concept's generalizations.  */

h61a_put_if_example(_,_,[],_).
h61a_put_if_example(Concept,Example,[Spec|Specs],Slot) :-
        get(Spec,[defn,name],[Defn]),
        getarity(Concept,Arity),
        makeinst(Defn,Arity,Example,Call),
        Call,
/*      apply(Defn,[Example]),        */
        fremove(Concept,[examples,typ],Example),

/*      get(Concept,[examples,dif],[Num,Time]),
        Dif is Num/Time,
        put(Spec,[examples,dif],[1,Dif]),
*/
        put_if_more_examples_needed(Spec,Slot,Example),
        h61a_put_if_example(Concept,Example,Specs,Slot).

h61a_put_if_example(Concept,Example,[Spec|Specs],Slot) :-
        h61a_put_if_example(Concept,Example,Specs,Slot).

/* put_if_more_examples_needed(Concept,Example) -
   Determines if Concept is important enough to store more examples of the
   Concept than is already stored, and if so, stores Example in Slot.
*/
put_if_more_examples_needed(Concept,Slot,Example) :-
        exs(Concept,Examples),
        length(Examples,Number),
        more_values_needed(Concept,Slot,Number),
        put(Concept,Slot,Example),
        addtoagenda(check,Concept,[examples,typ],100,'Check new examples').
put_if_more_examples_needed(_,_,_).

/* more_values_needed(Concept,Slot,Number) - 
   Determines by the worth of a Concept and its Slot if there is justification
   to store more values for the Concept's Slot.  Number is the current number
   of values stored for the Slot.
*/
more_values_needed(_,_,Number) :-
        Number < 15,!.
more_values_needed(Concept,Slot,Number) :-
        get(Concept,[worth],[Concept_worth]), 
        worth(Slot,Slot_worth),
        Value_limit is Concept_worth * Slot_worth / 4000,
        Number =< Value_limit,
        Number =< 60.

