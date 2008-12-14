/****************************************************************************
 * 
 * h92 specializes a concept definition. Each specialization is formed by
 * dropping a clause of the original concept definition.
 * Tasks are suggested to fill-in the frames for each new concept.
 * Many of the concepts created are worthless. The user gets a chance to
 * throw them out of the agenda before the frames are filled-in.
 * This heuristic generates as many new concepts as possible by using
 * 1/2 of the available time.
 */

h92(Concept):- get(Concept,[defn,name],[Mainfunctor]),
  getarity(Concept,N),
  collectclauses(Mainfunctor,N,Clauses),
  time(T),T1 is T/2, clock(Start,_),
  h92_do_while_time(Concept,Mainfunctor,T1,Start,Clauses).
h92(_).

/*** h92_do_while_time drops clauses from the concept definition 
**** of the passed parameter Concept. Each clause deletion results
**** in a new concept definition. This process is repeated until the
**** allocated time is used up (Alotment). Each new concept definition
**** is displayed to the user to allow for it's renaming or deletion.
**** If accepted by the user, the new concept definition is asserted.
**** Note the trick for iterating through h92_do_while_time: the
**** predicate times_up FAILS if there is time remaining and
**** succeeds is the alloted time is used up. Failure forces backtracking
**** resulting in alternative paths through dropclause. This
**** works since the effects of h92_do_while_time are achieved through
**** side-effects.
***/

h92_do_while_time(Concept,Mainfunctor,Alotment,Start,Clauses):- 
  dropclause(Clauses,TempClauses), newdefname(spec_of_,Concept,NewName),
  totalreplace(Mainfunctor,NewName,TempClauses,NewClauses), 
  not_already_defined(NewName,NewClauses),
  check_with_user(Concept,specialization,NewName,NewClauses,NewName2,NewClauses2),
  assertset(NewClauses2),
  updateconcepts(Concept,NewName2,spec),
  times_up(Alotment,Start).


/*** succeed only if the newly generated procedure is unique (can't unify with
**** any concept AM already knows about).  fails otherwise.
***/
not_already_defined(OldName,OldClauses):-
   frame(Name,[defn,name], [Name_defn]),
   getarity(Name,Arity),
   collectclauses(Name_defn,Arity,NameClauses),
   totalreplace(OldName,Name_defn,OldClauses,NewClauses),
   NameClauses = NewClauses,
   amformat('~n   (The new concept, ~a, is the same as the concept, ~a!)~n',[OldName,Name]),
   !,  fail.
not_already_defined(_,_).

/*** updateconcepts simply does the bookkeeping required for new concepts.
**** The name, arity and worth slots are filled-in and some tasks are
**** added to the agenda.
***/

updateconcepts(OldConcept,NewConcept,Relation):- 
  put(NewConcept,[name],NewConcept),
  ensure_name_ends_with__defn(NewConcept,NewConcept_defn),
  put(NewConcept,[defn,name],NewConcept_defn),
  put(OldConcept,[Relation],NewConcept),
  copyslot(OldConcept,NewConcept,[defn,arity]),
  copyslot(OldConcept,NewConcept,[worth]),
  addtoagenda(fillin,NewConcept,[examples],200,'no examples of this new concept'
),
  addtoagenda(fillin,NewConcept,[worth],200,'new concept with unknown worth'), !
.

copyslot(FromFrame,ToFrame,Slot):- 
  get(FromFrame,Slot,X), update(ToFrame,Slot,X).

/***
**** This is a new function added by adam,  Without this, the
**** new generalizations are not linked into the tree properly.
**** The result is that we cannot collect any heuristics for them.
***/

put_in_hierarchy(Con,Newcon) :-
        get(Con,[genl],Gens),
        remove(Newcon,Gens,Gens1),
        putvals(Newcon,[genl],Gens1),!.

/*** check_with_user is a simple but messy function for interacting
**** with the user. The intent is determine if the user likes a concept
**** definition and to determine a name for it.
***/

check_with_user(Concept,_,X,Y,X,Y):-
   assertz(gensymed_concepts(Concept)),  !.
check_with_user(Concept,Relation,NewConcept,NewClauses,NewConceptName,NewClauses2):- 
  nl,nl,
  write(' I have created a concept definition which is a '), write(Relation),
  write(' of'), nl,write(Concept),write('.'),
  write(' This new concept is defined as follows: '), nl,
  ppclauses(NewClauses), nl,
  repeat,
   write(' Do you want to keep this new concept (y/n)? '), 
   aminput(Reply), (Reply=y ; Reply=n ; Reply=''),
   !, (Reply=y ; Reply=''),
  repeat,
   write(' Please type new name for this concept or <CR> to keep current name:'),
   aminput(X),
   check_new_concept_name(X,NewConcept,NewConceptName,NewClauses,NewClauses2),
   !.

check_new_concept_name('',NewConcept,NewConcept,NewClauses,NewClauses):- !.
check_new_concept_name(X,NewConcept,X,NewClauses,NewClauses2):-
   ensure_name_ends_with__defn(X,X2),
   \+frame(_,[defn,name],X2),
   totalreplace(NewConcept,X2,NewClauses,NewClauses2), !.
check_new_concept_name(X,NewConcept,NewConceptName,NewClauses,NewClauses2):-
   amformat('The name ~a, is already being used.  Please try again.~n',[NewConcept]),
   fail.

ensure_name_ends_with__defn(Old,Old):- 
   name(Old,L),
   append(_,[95,100,101,102,110|_],L), !.
ensure_name_ends_with__defn(Old,New):-
   name(Old,L),
   append(L,[95,100,101,102,110],NewL),
   name(New,NewL).
   
ppclauses([]).
ppclauses([H|T]):- ppclause(H), ppclauses(T).
ppclause([Head|[Body]]):- write(Head),write(':'),write('-'),nl, 
  write('  '),write(Body), write('.'), nl.
    
/*** newdefname(X,Y,New) forms NEW by concatenating the following:
****     X, Y, _defn, I    where I is a generated integer.
***/

newdefname(Name1,Name2,NewName):- makename(Name1,Name2,Temp),
  makename(Temp,'_defn',Tempname), gensym(Tempname,NewName).

/*** dropcondition removes a term from Clauses to form NewClauses.
**** dropcondition will not remove the term 'basecase' which is intended
**** to flag the basecase clause of a recursive definition. By backtracking
**** different terms are selected for dropping. Note that Clauses must
**** be a list (of the form returned by collectclauses).
***/

dropcondition(Clauses,NewClauses):- member(Clause,Clauses),
  conditionof(Cond,Clause), \+Cond=basecase,
  removecond(Cond,Clause,NewClause), 
  replace(Clause,NewClause,Clauses,NewClauses).

removecond(Cond,Clause,NewClause):- Clause=[Head,Body],
  removecond2(Cond,Body,NewBody), NewClause=[Head,NewBody].
removecond2(Cond,Cond,true).
removecond2(Cond,(Cond,Y),Y).
removecond2(Cond,(X,Cond),X).
removecond2(Cond,(X,Y),Z):- \+X=Cond, \+Y=Cond, 
  removecond2(Cond,Y,Z).

/*** conditionof returns a single term (Cond) of Clause. The only
**** tricky part is getting past the functors ',' and ';'.
**** Note that Clause must be in the form (Head,Body). A null
**** body is encoded as 'true'.
***/

conditionof(Cond,Clause):- Clause=[_,Body], conditionof2(Cond,Body).
conditionof2(Cond,Cond):- \+Cond=true, Cond=..[F|_], \+F=(','), \+F=(';').
conditionof2(Cond,(X,Y)):- conditionof2(Cond,X).
conditionof2(Cond,(X,Y)):- conditionof2(Cond,Y).

/*** assertset does the inverse of collectclauses: given a list of
**** clauses, assertz each clause to the database. Note that assertz
**** is used to maintain order of the clauses.
***/

assertset([]).
assertset([Clause|Rest]):- Clause=[Head|[true]], assertz((Head)),
  assertset(Rest).
assertset([Clause|Rest]):- Clause=[Head|[Body]], assertz((Head:-Body)),
  assertset(Rest).

/*** dropclause removes a clause from a set of clauses. Note that
**** the basecase clause is not deleted. (The term 'basecase' is
**** added to the basecase clause of a recursive definition as an
**** AM/p convention).
***/

dropclause(Clauses,NewClauses):- member(Clause,Clauses), droppable(Clause),
  remove(Clause,Clauses,NewClauses), \+NewClauses=[].
droppable(Clause):- \+ conditionof(basecase,Clause).

/*** totalreplace(+Oldfunctor,+Newfunctor,+Oldclauses,-Newclauses)
**** replaces all terms with functor=Oldfunctor with Newfunctor
**** in the clauses Oldclauses to produce Newclauses. Note that
**** Oldclauses must be in the form produced by collectclauses.
**** totalreplace must check both the head and the body of each
**** clause for occurrences of Oldfunctor
***/

totalreplace(Old,New,Old,New).
totalreplace(Old,New,Oldstruc,Oldstruc):- atomic(Oldstruc), \+Old=Oldstruc.
totalreplace(Old,New,[OldHead|OldTail],[NewHead|NewTail]):-
  totalreplace(Old,New,OldHead,NewHead),
  totalreplace(Old,New,OldTail,NewTail).
totalreplace(Old,New,Oldstruc,Newstruc):- 
  \+atomic(Oldstruc),
  Oldstruc=..[Oldhead|Oldargs], \+Oldhead='.',
  totalreplace(Old,New,Oldhead,Newhead),
  Newstruc=..[Newhead|Oldargs].

