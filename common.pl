:- public([
        times_up/2, collectclauses/3, makelist/2,
        ok_a/2,termination_a/3,cleanup_a/2,addnum_a/1,
        check_if_any_found/2,unifyinst/4,

        allconcepts/1,fillable_slots/1,examples/2,exs/2,
        isas/2,genls/2,specs/2,ripple/3,collect/3,
        genls_sf/2,specs_sf/2,ripple_sf/3,getarity/2,
        get/3,concept/1,put/3,putvals/3,update/3,fremove/3,
        fremoveall/2,ppframe/1,ppall/0,worth/2,aminput/1]).

%Use examine/0 to browse the concept base
examine:-
   write('EXAMINE which concept?  (completion supported)  '),
   aminput(ConToken), 
   \+member(ConToken,['',q,quit,e,exit]),
   name(ConToken,ConString),
   frame(Name,_,_),   name(Name,FrameString),
   append(ConString,_,FrameString),	% isa match?
   ppframe(Name),
   !, examine.


/*** times_up succeeds if the elapsed time exceeds the alloted  time;
**** else fail.
***/

times_up(Alotment,Start):-  clock(Start,T),T>Alotment.

/*** collectclauses forms a list of all clauses with a given mainfunctor.
**** The only tricky part is forming a template which will match the
**** head of each of the clauses (this to satisfy the 'clause' predicate).
***/

collectclauses(Mainfunctor,N,Clauses):- functemplate(Mainfunctor,N,Func),
  bagof([Func,Body], clause(Func,Body), Clauses).
functemplate(Mainfunctor,N,Func):- makelist(N,L), Func=..[Mainfunctor|L].

/*** makelist(+N,-L) forms a list L of length N of uninstantiated variables. */

makelist(0,[]).
makelist(N,[_|L]):- N>0, N1 is N-1, makelist(N1,L).


/*************************************************************
 * The following set of functions are used to control a
 * heuristics which uses backtracking, and produces results
 * through side_effects.
 */

ok_a(Start,Allot) :- clock(Start,T), Allot > T.
ok_a(Start,Allot) :- clock(Start,T), Allot > T,ok_a(Start,Allot).

makenumcall(H,Var,Call) :- makename(H,num,Funct), Call =.. [Funct,Var].
        
termination_a(H,_,_) :- makenumcall(H,Num,Call),Call,Num>24.
termination_a(_,S,T) :- times_up(S,T).

cleanup_a(H,Num) :- makenumcall(H,Num,X),retract(X),!.
cleanup_a(_,0).

addnum_a(H) :- makenumcall(H,Num,Term), retract(Term),
        N is Num +1,makenumcall(H,N,New),asserta(New),!.
addnum_a(H) :- makenumcall(H,1,Term),
        asserta(Term).

check_if_any_found(H,C) :- makenumcall(H,Num,Call),Call,integer(Num),
        Num > 0,
        addtoagenda(check,C,[examples,typ],200,'have  found some exs of C').

/******************** END OF SET **********************************/


/******************************************************
 *      unifyinst is the result of an inconsistency
 *      in our treatment of examples.  The example of an 
 *      object is different from an example of an activity.
 *      I.e. objects have an arity of 1, with NO dom/ran.
 *      So we have to unify them differently to get the
 *      appropriate call.
 */
unifyinst(1,Inst,Mainfunctor,Ex) :- Inst =.. [Mainfunctor,Ex],!.
unifyinst(_,Inst,Mainfunctor,Ex) :- Inst =.. [Mainfunctor|Ex].

makeinst(Defn,1,Ex,Call) :- Call =.. [Defn,Ex],!.
makeinst(Defn,_,Ex,Call) :- Call =.. [Defn|Ex].

%%  End of COMMON STUFF  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* return all currently defined concepts */
allconcepts(C) :-
        setof(X,Y^frame(X,[name],Y),C).

/* return all slots that AM has heuristics to fillin */
fillable_slots(S) :-
        S = [[defn],[examples],[genl],[spec],[alg],[dom_range],
                [in_domain_of],[in_range_of],[isas]].
/* collect X.examples since they may live on several sub-slots */
examples(Con,L) :- 
        get(Con,[examples,bnd],L1),    /* bound changed to bnd, KM 7/24 */
        get(Con,[examples,typ],L2),
        append(L1,L2,L).

/* collect all the examples of Con by collecting the examples
 * of all the specs of Con
 */
exs(Con,Examples):-
   specs_sf(Con,Specs),		       % spec*(X)=Specs
   removedups(Specs,Specs1),
   exs1(Specs1,Exs),                   % examples(Specs)=Exs
   exs2(Exs,Examples),!.               % spec*(Exs)=Examples


exs1([],[]).
exs1([H|T],L) :- examples(H,L1),exs1(T,L2), union(L1,L2,L).

exs2([],[]).
exs2([H|T],L) :- ripple(down,H,L1),exs2(T,L2),union(L1,L2,L).

/* collect the isas according to the formula:
 *      genl*(isa(genl*(X)))
 */
isas(Con,L1) :- genls_sf(Con,G),
        collect([isas],G,Isas1),
        removedups(Isas1,Isas2),
        isas1(Isas2,L),
        removedups(L,L1).

isas1([],[]).
isas1([H|T],L) :- genls_sf(H,L1),isas1(T,L2),append(L1,L2,L).

/* collect genls or specs of a concept X by rippling UP or Down in
 * the hierarchy
 *      ripple(+direction,+concept,-list of concepts)
 */

genls(Con,G) :- ripple(up,Con,G),!.

specs(Con,S) :- ripple(down,Con,S),!.

ripple(up,X,Genls) :- ripple1([genl],[X],G),Genls = [X|G].
ripple(down,X,Specs) :- ripple1([spec],[X],G),Specs = [X|G].
ripple(_,X,[X]).

ripple1(_,[],[]):-!.
ripple1(Dir,X,G) :- adjacent_to(Dir,X,G1),
        ripple1(Dir,G1,G2),
        append(G1,G2,G).

/* ripple_sf is a safe ripple, that can deal with loops in the tree 
 * similarly with genls_sf and specs_sf
 */
genls_sf(C,G) :- ripple_sf(up,C,G),!.
specs_sf(C,S) :- ripple_sf(down,C,S),!.

ripple_sf(up,X,Genls) :-   ripple_sf1([genl],[X],[X],G),Genls = [X|G], !.
ripple_sf(down,X,Specs) :- ripple_sf1([spec],[X],[X],G),Specs = [X|G], !.
ripple_sf(_,X,[X]).

ripple_sf1(_,_,[],[]):-!.
ripple_sf1(Dir,Seen,Level,G) :-
        adjacent_to(Dir,Level,Nextlevel),
        setdif(Nextlevel,Seen,Neverseen),
        append(Seen,Neverseen,Nowseen),
        ripple_sf1(Dir,Nowseen,Neverseen,G1),
        append(Neverseen,G1,G).

adjacent_to(_,[],[]):-!.
adjacent_to(Dir,[H|T],G) :- get(H,Dir,G1),
        adjacent_to(Dir,T,G2),
        append(G1,G2,G).

/* collect all the entries on Slot for each concept in List
 *      collect(+Slot,+List_of_cons,-List_of_vals)
 */
collect(_,[],[]):-!.
collect(Slot,[H|T],L) :-
        get(H,Slot,L1),
        collect(Slot,T,L2),
        append(L1,L2,L).

/* returns the arity of a concept definition */
getarity(Con,Arity) :-
        get(Con,[dom_range],[L|_]), length(L,Arity).
getarity(Con,Arity) :- get(Con,[defn,arity],[Arity]).

concept(C) :- frame(C,_,_),!.

get(Name,Slot,Value):- frame(Name,Slot,Value),!.
get(_,[worth],[0]).
get(_,[examples, dif],[0,0]).
get(_,_,[]).
% commented out because they don't work (in my opinion).
%get(Name,Slot,Value) :- nonvar(Value), !,
%       get1(Name,Slot,Value).
%get1(Name,Slot,Value) :- frame(Name,Slot,X),!,X = Value.
%get1(Name,Slot,[]).
%to put a single element onto a slot

% the first clause catches all changes to the concept base and displays them.
%? put(C,S,V):-
%?    watch_mode_on,				% if you want a trace,
%?    print_put_trace(C,S,V).  % ! This will fail and backtrack to the real put/3.
put(C,S,V) :- put1(C,S,V),!.

/*  This stuff is duplicated in am.pl so it will be interpreted because it
    doesn't work if it's compiled.  (another FEATURE of VAX/VMS QP) */
print_put_trace(C,S,V):-
   ancestors([G|_]),				% find out who's calling put/3,
   G=..[H|_],
   writeln(['  ',H,'is adding',V,to,the,S,slot,of,C,nl]), % show change,
   !, fail.

put1(C,[examples,dif],[N,T]) :- !,nonvar(N),nonvar(T),
        ((retract(frame(C,[examples,dif],[N1,T1])),
        N2 is N + N1, T2 is T + T1,
        assertz(frame(C,[examples,dif],[N2,T2])))
        ;
        (assertz(frame(C,[examples,dif],[N,T])))).
put1(C,[spec],V) :-
        put2(C,[spec],V),
        put2(V,[genl],C).
put1(C,[genl],V) :-
        put2(C,[genl],V),
        put2(V,[spec],C).
put1(C,[worth],W) :-
        update(C,[worth],[W]).
put1(C,[dom_range],D_r) :-
        put_d_r(C,D_r),
        put2(C,[dom_range],D_r).
put1(C,[isas],V) :-
        put2(C,[isas],V),
        put2(V,[examples,typ],C).
put1(C,[examples,T],V) :-
        concept(V),
        member(T,[bnd,typ]),
        put2(C,[examples,T],V), 
        put2(V,[isas],C).
put1(C,[conjecs],X) :-
        put2(C,[conjecs],X),
        put1(conjecs,[examples,typ],X).
        
put1(C,S,V) :- put2(C,S,V).

put2(Name,Slot,Item) :- frame(Name,Slot,Value),
        member(Item,Value).
put2(Name,Slot,Item) :- retract(frame(Name,Slot,Value)),
                assertz(frame(Name,Slot,[Item|Value])).
put2(Name,Slot,Item) :- assertz(frame(Name,Slot,[Item])).

put_d_r(C,[R]) :- put(R,[in_range_of],C).
put_d_r(C,[D|R]) :- put(D,[in_domain_of],C),put_d_r(C,R).

%to add several vals to a slot
putvals(_,_,[]).
putvals(C,S,[H|T]) :-
        putvals(C,S,T),
        put(C,S,H).

%update - to replace oldvalue with newvalue
update(Name,Slot,Newval) :- retract(frame(Name,Slot,_)),
        assertz(frame(Name,Slot,Newval)).
update(Name,Slot,Newval) :- assertz(frame(Name,Slot,Newval)).

%fremove - remove item from values of slot.  Fail if not present
fremove(C,S,V) :- fremove0(C,S,V),!.

fremove0(C,[genl],V) :-
        fremove1(C,[genl],V),
        fremove1(V,[spec],C).
fremove0(C,[spec],V) :-
        fremove1(C,[spec],V),
        fremove1(V,[genl],C).
fremove0(C,[isas],V) :-
        fremove1(C,[isas],V),
        fremove1(V,[examples,typ],C).
fremove0(C,[examples,T],V) :- concept(C),member(T,[typ,bnd]),
        fremove1(C,[examples,T],V),
        fremove(V,[isas],C).

fremove0(C,S,V) :- fremove1(C,S,V).

fremove1(Name,Slot,Item) :- frame(Name,Slot,Val),
        remove(Item,Val,Newval),
        update(Name,Slot,Newval).
fremove1(_,_,_).

% remove the entire slot.
fremoveall(Name,Slot) :- retract(frame(Name,Slot,_)).
fremoveall(_,_).
  
/* print all concept frames to current stream */

ppall :-
        allconcepts(X), 
        member(A,X),
        ppframe(A),nl,
        fail.
ppall.

/* print a frame and the values on its slots */
ppframe(X) :- 
        mysetof((Y,Z),frame(X,Y,Z),L),
        remove(([name],Names),L,L1),
        write(X),write(': '),myprint(Names,_),nl,
        ppframe1(L1),nl,!.

ppframe1([(Slot,Vals)|R]) :- 
        tab(3), myprint(Slot,Len),write(':'), Col is 3 + Len,
        ((Col < 25,tab(25 - Col))
        ;true),
        tab(3), print(Vals),
        nl,ppframe1(R).
ppframe1([]).


/* in addition to the worth of a concept, there is an apriori worth
 * given to each operation and slot.
 */
worth(A,W) :- frame(A,[worth],[W]).
worth(Operation,300) :- member(Operation,[fillin,check,int,suggest]).
worth(Slot,300).


aminput(X):-  write('>>'),ttyflush,myinput(X).

