:- public(h31/1).

/***********************************************************
 * h31 anyconcept.examples.fillin
 *      
 *      unfolds the definition of a RECURSIVE defn of C
 *      if the base case is of the form 
 *              c_defn(a,b,..) :- basecase.
 *      i.e. there are no tests.  The problem with tests is that
 *      You would have to do an extensial proof of some X that
 *      satisfied them.  
 *      The game plan is to collect the clauses for c_defn,
 *      find an instance of a call to c_defn by using existing
 *      examples or using the basecase.
 *      Next remove the base case from the clauses, and bind
 *      the instance to the recursive call.  This way, when
 *      we execute the defn, the recursive call is already 
 *      instantiated and its variable bindings are propagated
 *      to the other terms.
 */

h31(C) :-
        getarity(C,Arity),
        get(C,[defn,name],[Mainfunctor]),
        exs(C,Exs),
        collectclauses(Mainfunctor,Arity,Clauses),
        time(T1),
        T is T1 / 2,    % use 1/2 of remaining time
        clock(S,_),
        h31_do_while_time(T,S,C,Mainfunctor,Arity,Clauses),
        clock(S,Time),
        cleanup_a(h31,Num),
        put(C,[examples,dif],[Num,Time]),
        ((Num > 0,
          addtoagenda(check,C,[examples,typ],200,'have  found some exs of C'))
        ; !,fail).

h31_do_while_time(T,S,C,Mainfunctor,Arity,Clauses) :- 
        removebase(Clauses,Newclauses),
        ok_a(S,T),
        exs(C,Exs),
        findinst(Exs,C,Mainfunctor,Arity,Clauses,Inst), 
        unfold(Inst,Newclauses,Newinst),
        unifyinst(Arity,Newinst,_,NewEx),
        put(C,[examples,typ],NewEx),
        addnum_a(h31),
        termination_a(h31,S,T).
h31_do_while_time(_,_,_,_,_,_).


/****************************************************
 * findinst(Exs,Mainfunctor,Arity,Clauses,-Inst).
 *  Find an instance of the recursive call. 
 *  if there are no exs, then use the base case,
 *  otherwise, use the exs.
 */
findinst([],C,Mainfunctor,Arity,Clauses,Inst) :-
        functor(Inst,Mainfunctor,Arity),
        bindbase(Inst,Clauses),
        unifyinst(Arity,Inst,_,Ex),
        put(C,[examples,bnd],Ex),
        addnum_a(h31),!.

findinst(Exs,_,Mainfunctor,Arity,_,Inst) :-
        randomelement(Exs,Ex),
        unifyinst(Arity,Inst,Mainfunctor,Ex),!.



/*************************************************************
 * bindbase(+Inst,+Clauses)
 * Works by unifying Inst with the basecase of clauses.
 *
 * Note that the above clause for bindbase will ONLY work
 * if there are no TESTS on the base case.  The problem
 * is clear in the example:
 *      a(X) :- integer(X).
 * To instantiate this, we would have to run integer backwards.
 * It is even more akward with multiple clauses.  One could put 
 * some more restrictions on the tests which could be make
 * limiting them to ones which are invertible.  But this was
 * easier.  
 */
bindbase(Inst,Clauses) :- member([Inst,basecase],Clauses).
bindbase(Inst,Clauses) :- member([Inst,(basecase ',' _)],Clauses).

/********************************************************************
 * removebase(+Clauses,-Nonbase)
 * deletes the basecase clause from clauses -- look at note about
 * bindbase.
 */
removebase(Clauses,Nonbase) :- remove_or_die([_,basecase],Clauses,Nonbase),!.
%removebase(Clauses,Nonbase) :- remove([_,(basecase ',' _)],Clauses,Nonbase),!.

/******************************************************************
 *       unfold(+Instance,+Clauses,-Head)
 * Binds Head to a new instance of the predicate we are unfolding.
 * It does this by binding the recurisive calls in Clauses to
 * Instance. Then it trys to execute each clause in Clauses.
 * If one of them succedes, Head will be bound to the head of 
 * that clause.  E.g.
 *      unfold(a([]),[[a(X),fail],[a([b|Y]),(somepred(X,Y),a(Y))]],H)
 * would bind H = a([b]).  First it would replace a(Y) with a([]),
 * binding Y = [].  Then member would pull of the first clause,
 * it would fail.  On backtracking, Body = (somepred(X,[]),a([])).
 * Suppose Body succedes, then unfold sucedes with H=a([b|[]]).
 *
 */
unfold(Inst,Clauses,Head) :-
        bind_recursive_call(Inst,Clauses),
        member([Head,Body],Clauses),
        Body,!.

/************************************************************
 * bind_recursive_call(+inst,+clauses) 
 * binds all recursive calls in clauses to Inst.
 *
 * Note that this could cause some trouble if Inst contained
 * some unbound vars, and the recursive calls instantiated them.
 */
bind_recursive_call(Inst,[]).
bind_recursive_call(Inst,[[_,Terms]|Rest]) :- 
        matchcall(Inst,Terms),
        bind_recursive_call(Inst,Rest).

/***********************************************************
 * matchcall(+Inst,+Terms)
 * matchcall binds inst to all recursive calls in each 
 * set of terms.
 */
matchcall(Inst,(Inst)).
matchcall(Inst,(X ',' Y)) :- matchcall(Inst,X),matchcall(Inst,Y).
matchcall(Inst,(X ';' Y)) :- matchcall(Inst,X),matchcall(Inst,Y).
matchcall(Inst,(_)).

