%break% mutate.pl       527388122   409   20    100644  16899     `

:-public([h89/1, h92/1, h300/1, h301/1]).

/****************************************************************************
 * 
 * h89 generalizes a concept definition. Each generalization is formed by
 * dropping a condition from a clause of the original concept definition.
 * Tasks are suggested to fill-in the frames for each new concept.
 * Many of the concepts created are worthless. The user gets a chance to
 * throw them out of the agenda before the frames are filled-in.
 * This heuristic generates as many new concepts as possible by using
 * 1/2 of the available time.
 */

h89(Concept):- get(Concept,[defn,name],[Mainfunctor]), 
  getarity(Concept,N),
  collectclauses(Mainfunctor,N,Clauses),
  time(T),T1 is T/2, clock(Start,_),
  h89_do_while_time(Concept,Mainfunctor,T1,Start,Clauses).
h89(_).

/*** h89_do_while_time drops conditions from the concept definition 
**** of the passed parameter Concept. Each condition deletion results
**** in a new concept definition. THis process is repeated until the
**** allocated time is used up (Alotment). Each new concept definition
**** is displayed to the user to allow for it's renaming or deletion.
**** If accepted by the user, the new concept definition is asserted.
**** Note the trick for iterating through h89_do_while_time: the
**** predicate times_up FAILS if there is time remaining and
**** succeeds is the alloted time is used up. Failure forces backtracking
**** resulting in alternative paths through dropcondition. This
**** works since the effects of h89_do_while_time are achieved through
**** side-effects.
***/

h89_do_while_time(Concept,Mainfunctor,Alotment,Start,Clauses):- 
  dropcondition(Clauses,TempClauses), newdefname(gen_of_,Concept,NewName),
  totalreplace(Mainfunctor,NewName,TempClauses,NewClauses), 
  not_already_defined(NewName,NewClauses),
  check_with_user(Concept,generalization,NewName,NewClauses,NewName2,NewClauses2),
  assertset(NewClauses2),
  updateconcepts(Concept,NewName2,genl),
  put_in_hierarchy(Concept,NewName2),
  times_up(Alotment,Start).
