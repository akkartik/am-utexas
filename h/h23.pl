/********************************************************
 *                      h23                             *
 *      Concept C is interesting if each example of C   *
 *      accidentally seems to satisfy the otherwise-    *
 *      rarely satisfied predicate P, or (equivalently) *
 *      if there is an unusual conjecture involving C.  *
 ********************************************************/

/* I am going to take "unusual" conjecture to mean any
 * conjecture -- conjectures aren't made unless they are
 * interesting and unusual anyway.
 */

/* Note also that I do not know what is going to be the format
 * of the conjecs slot.  For the present, I am going to presume
 * that it is a list of elements, one member of which may be
 * the concept Con that we are dealing with.
 */

h23(Con) :- 
        allconcepts(All_cons),
        collect(conjecs,All_cons,Jecs_list),
        h23_aux(Con,Jecs_list).

h23_aux(X,[]) :- fail.
h23_aux(X,[A|B]) :-
        (member(X,A) ; h23_aux(X,B)).

/* If in the [examples,bnd] slot of the concept 'Predicate'
 * we were to store those predicates that are rarely satisfied,
 * then we could easily access rarely-satisfied predicates and
 * deal with the first part of this heuristic.  Failing this
 * slot, it doesn't seem worth it to searching through ALL
 * predicate evaluations and seeing which ones aren't satisfied
 * very often.
 */

