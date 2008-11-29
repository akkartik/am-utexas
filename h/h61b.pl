/* H61b - Any-concept.Examples,typical.Check :

   *** Encodes H59, H61, and H62 for boundary examples

   Ensure boundary examples satisfy the Concept's definition, then see if they 
   also satisfy any specialization definition.  If so, then remove them from 
   the Concept's boundary examples list and add them to those specializations'
   typical examples lists.  When any example fails to satisfy the Concept's 
   definition then remove it from the Concept's examples list and see if it
   satisfies any generalization of the concept.  If so, then add it to the 
   boundary examples lists of those generalizations.                       */

h61b(Concept) :-
        time(Time),
        Allowed is Time/3,
        clock(Start,_),
        !,
        get(Concept,[defn,name],[Defn]),
        h34a_find_values([spec],Concept,Specs),
        h34a_find_values([genl],Concept,Genls),
        h34a_find_values([examples,bnd],Concept,Examples),
        h61b_do_while_time(Start,Allowed,Concept,Defn,Examples,Specs,Genls).

/* h61b_do_while_time(Start,Allowed,Concept,Defn,Examples,Specs,Genls) -
   While time remains, h61b_do_while_time processes each example of Concept
   in Examples, ensuring that it is appropriately included as an example of
   Concept and should not be moved to the examples list of some specialization
   or generalization of Concept.                                            */

h61b_do_while_time(_,_,_,_,[],_,_).
h61b_do_while_time(Start,Allowed,_,_,_,_,_) :-
        clock(Start,Elapsed),
        Elapsed > Allowed.
h61b_do_while_time(Start,Allowed,Concept,Defn,[Example|Examples],Specs,Genls):-
        getarity(Concept,Arity),
        makeinst(Defn,Arity,Example,Call),
        Call,
%       apply(Defn,[Example]),
        h61b_put_if_example(Concept,Example,Specs,[examples,typ]),
        h61b_do_while_time(Start,Allowed,Concept,Defn,Examples,Specs,Genls).
h61b_do_while_time(Start,Allowed,Concept,Defn,[Example|Examples],Specs,Genls):-
        fremove(Concept,[examples,bnd],Example),
        h61b_put_if_example(Concept,Example,Genls,[examples,bnd]),
        h61b_do_while_time(Start,Allowed,Concept,Defn,Examples,Specs,Genls).

/* h61b_put_if_example(Concept,Example,Specs,Slot) -
   Specs is either the generalizations or the specializations of Concept.
   Example is a boundary example of Concept, and is tested to see if it
   satisfies any of the concepts in Specs.  When any such member of Specs
   is found, Example is removed from the examples list of Concept and is
   added to the Slot examples list of that concept from Specs.  Slot is
   assumed to be [examples,typ] when specs are Concept's specializations;
   Slot is [examples,bnd] when Specs are Concept's generalizations.
*/
h61b_put_if_example(_,_,[],_).
h61b_put_if_example(Concept,Example,[Spec|Specs],Slot) :-
        get(Spec,[defn,name],[Defn]),
        getarity(Concept,Arity),
        makeinst(Defn,Arity,Example,Call),
        Call,
%       apply(Defn,[Example]),
        fremove(Concept,[examples,bnd],Example),
        put_if_more_examples_needed(Spec,Slot,Example),
        h61b_put_if_example(Concept,Example,Specs,Slot).
h61b_put_if_example(Concept,Example,[Spec|Specs],Slot) :-
        h61b_put_if_example(Concept,Example,Specs,Slot).

