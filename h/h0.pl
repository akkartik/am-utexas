/* Some of these heuristics were derived from Lenat'st thesis. They were
   not explicitly listed though. They a start at 402.


*/ 

/* if the number of examples of C are between 5 and 30 increment worth.
   This is Lenat's no too many not too few */


h402(C):- 
	(exs(C,Examples);get(C,[examples,typ],Examples)),
        length(Examples,N),
        N > 5, N < 30,
        (retract(frame(C,[worth],[Worth]));Worth = 100),
        Worth2 is Worth + (Worth + 1)/5,
        assertz(frame(C,[worth],[Worth2])).

h402(C):- 
	(exs(C,Examples);get(C,[examples,typ],Examples)),
        length(Examples,N),
        N < 5,
        (retract(frame(C,[worth],[Worth]));Worth = 1),
        Worth2 is Worth/2,
        assertz(frame(C,[worth],[Worth2])).



h402(C):- 
	(exs(C,Examples);get(C,[examples,typ],Examples)),
        length(Examples,N),
        N > 40,
        (retract(frame(C,[worth],[Worth])); Worth = 1),
        Worth2 is Worth/2,
        assertz(frame(C,[worth],[Worth2])).



/* if a concept is worthwhile then compose it with itself; this is short
of like Lenat's repetition heuristic. However, the only way that that
heuristic will work is if the domain = range! Thus one could be equal to
repetitive application of a concept and it might not. -marcos */

h407(F):- 
	get(F,[worth],[Worth]),
        Worth > 200,
        assertz(flag),
        getarity(F,N1), N is N1 -1,
        loop_composit(F,F,N,[],Glist,[],FoGdr,1),
        makename(F,'_o_',Temp),
        loopmakename(Temp,Glist,SeedName),
        loop_make_composit(F,SeedName,Glist,FoGdr,Newname,Algorogo,N,0),!,
        assertz(flag),
        create_composite_concept2(F,Glist,Newname,Algorogo,FoGdr).        

loop_composit(F,G,N,X,X,Y,Y,_):-
        N = 0.

loop_composit(F,G,N,Glist,New_Glist,Old_FoGdr,New_FoGdr,It) :-
        get(F,[dom_range],Fdr),
        get(F,[dom_range],Gdr),
        get_composite_dr(Fdr,Gdr,FoGdr,It),
        N1 is N - 1,
        It2 is It + 1,
        loop_composit(F,F,N1,[G|Glist],New_Glist,[FoGdr|Old_FoGdr],
		      New_FoGdr,It2).

/* this is another attemp to have a heuristic that makes multiple attempts
at doing something repetitively. I am assuming that the number of
repetitions it not crucial. And of course it runs both forwards and
backwards!  It also runs with just one var as input. That is to say
the above applies to the defns and the algs that this heuristic
creates! -marcos */


h408(C):-
	get(C,[worth],[Worth]),
	Worth >= 100,
	get(C,[defn,name],[CON_NAME]),
        getarity(C,N),
	do_while_time(C,CON_NAME,Worth,N).

do_while_time(C,Defn_Name,Worth,N):-
        Times is Worth/10,
	makename(Defn_Name,'_X_',Temp),
	makename(Temp,Times,NewC_Dname),
	makeit(Defn_Name,N,Worth,Alg,NewC_Dname),
	get(C,[dom_range],Dom_range), Conceptname = NewC_Dname,
/*	check_with_user(),*/
	write(Alg),
        create_composite_concept(C,NewC_Dname,Alg,Dom_range).
	
create_composite_concept(F,Conceptname,Alg,Dom_range):-
        put(Conceptname,[name],Conceptname),
        put(Conceptname,[defn,name],Conceptname),
        put(Conceptname,[alg],Conceptname),
        assertz(frame(Conceptname,[dom_range],Dom_range)),
        put(Conceptname,[genl],F),
        get(F,[worth],[W1]),
        put(Conceptname,[worth],W1),
        addtoagenda(fillin,Conceptname,[examples],W1,
                    'no examples of this new concept').



makeit(D,N,Worth,Alg,Name):-  
	Count is Worth/10,
	makelist(N,List),
        split_last(List,Domain,Range),
	split_last(Domain,D1,D2),
	append(Domain,[D2],List2),
	append(Domain,[OutPut],List4),
	append(D1,[Range,OutPut],List3),
	Pred1 =.. [D|List],
	Pred2 =.. [Name|List2],
	Pred3 =.. [Name|List3],
	Pred4 =.. [Name|List4],
        makename(Name,'count',NewName),
	CP1 =.. [NewName|[Count1]],
	CP2 =.. [NewName|[Count2]],

        C0 = (Pred2:- CP1,Count1 > Count, retract(CP2)),
	C1 = (Pred4:- Pred1, (retract(CP1);
	                      Count1 = 0), Count2 is Count1 + 1, 
			      assertz(CP2),
	                      Pred3),
        assertz(C1),
	asserta(C0),nl, write(C0),nl, write(C1),nl,Alg=[C0,C1].




/*
callit(D,[],WholeExs,N,Output,Count,Worth,Start,LastR):-
	End = Start + 20,
        clock(End,End2),
        End2 < 0,
	callit(D,WholeExs,WholeExs,N,Output,Count,Worth,Start,LastR).

callit(Dname,[E1|Rest],WholeExs,N,Output,Count,Worth,Start,LastR):-
	End = Start + 10,
        clock(End,End2),
        End2 < 0,
	split_last(E1,Range,Dom),
        ((var(LastR),Range2 = Range);
         (split_last(Range,[R1],R2),
	  append(R1,LastR,Range2)))
	append(Range2,[Dom2],RDList),
        Pred =..[Dname|RDList],
	Count2 is Count + 1,
	callit(Dname,Rest,WholeExs,N,Output,Count2,Worth,Start,Dom2).

callit(Dname,_,_,N,Output,Count,Worth,Start,Output):-
	End = Start + 10,
        clock(End,End2),
        End2 >= 0.




         */
        

        
/* This function is the general inverse function. It's a little funky 
because most of the prolog defn's and alg's don't really run perfectly
backwards and forwards, even though I debugged them alot. So I had
to put in a few caviats into this heuristic. And of course this is
not a perfect inverse because there are many ways to do an inverse for
an N-ary function. Or so it seems to me. Though  calls like :

| ?- inverse_of_set_insert_defn([a],a,P).

P = [] 

| ?- inverse_of_set_insert_defn([a],B,P).

B = a,
P = [] 

Produce the right results calls like:

yes
| ?- inverse_of_set_insert_defn([a],[],P).

no
| ?- 
So I have h409 make, for arity = 3, and perhaps greater, simply the reverse
of the dom_range list!

-marcos
*/

h409(C):- 
	get(C,[defn,name],[DN]),
	getarity(C,N),
	makelist(N,List),
	split_last(List,Dom,Range),
        ((N > 2,
	split_last(Dom,D1,D2),
        append([Range],D1,RD1),
	append(RD1,[D2],RD)); append([Range],Dom,RD)),
	makename('inverse_of_',DN,Nname),
	Pred1 =.. [Nname|RD],
	Pred2 =.. [DN|List],
	Alg = (Pred1:-Pred2,((N>2,not(Range = D2));
                             (N=2,not([Range] = Dom)))),
	((N >= 3,
	 reverse(List,Rlist),
         Pred3 =.. [Nname|Rlist],
 	 Alg2 = (Pred3:-Pred2,not(Range = D2)),
	 assertz(Alg2));true),
	asserta(Alg),nl,write(Alg),nl,write(Alg2),nl.




not(X):-X,!,fail.
not(X).


/* this is a gag for AM -marcos*/

check_with_user(Concept,Relation,NewConcept,NewClauses,NewConceptName,NewClauses2):- not(if_Flag),
	matchtoname(NewClauses,Cname),
	assertz(gensymed_concepts(Concept,NewConcept,Relation,Cname)),
	NewClauses2 = NewClauses, NewConcept = NewConceptname.
matchtoname([[C|T]],C).



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


run_I_face:- assertz(if_Flag),
	gensymed_concepts(Concept,NewConcept,Relation,OldName),
	check_with_user(Concept,Relation,NewConcept,NewClauses,NewConceptname,
	NewClauses2),
	retract(if_Flag),
	(var(NewConceptname); 
	 retract(frame(OldName,[name],[_])),
	 retract(frame(OldName,[defn,name],[_])), 
	 retract(frame(OldName,[worth],[Worth])),
	 Worth2 is Worth + 100,
	 put(Concept,[name],NewConceptname),
	 put(Concept,[defn,name],[NewClauses2]),
	 put(Concept,[worth],Worth),
	 retract(agenda(A)),
	 inc_tasks(Concept,NewConceptname,A,100,[],P)),
	 subCname(Concept,NewConceptname).
	 

inc_tasks(C,Cn,[],W,O,O).
inc_tasks(C,Nc,A,W,O,P):-
	split(A,[[L,C,L2,W1,L3]|Tail]),
	W2 is W1 + W,
	inc_tasks(C,Tail,W,[[L,Nc,L2,W2,L4]|O],P),
	assertz(agenda(P)).
	
/*check_with_user(Concept,Relation,NewConcept,NewClauses,NewConceptName,NewClauses2):- 
      assertz(gensymed_concepts(Concept,NewClauses,Relation)), 
	NewClauses2 = NewClauses.

*/

/*If C1 is a genl of C2 if C2 is a fenl of C3 ... if Ck is a genl of Cn then
merge and increase the value of the highest value to begin with*/

h114(C):- assertz(counter(0)),h114_it(C,C,0,[]).
h114a(C):-h114_ita(C,C,0,[]).

h114_it(Present_C,C,Counter,CTrail):-
         Counter < 100,
         retract(counter(_)),
         assertz(counter(Counter)),
         get(Present_C,[genl],Value),!,
         notmember(C,Value),!,
         first_element_defn(Value,FirstElement),
         counter(Counter),
         New_counter is Counter + 1,
         h114_it(FirstElement,C,New_counter,[FirstElement|CTrail]).
         
h114_it(Present_C,C,Counter,CTrail):-
         get(Present_C,[genl],Value),
         member(C,Value),!,
         rid_ex_cons(C,[Present_C|CTrail]).

h114_ita(Present_C,C,Counter,CTrail):-
         Counter < 100,
         retract(counter(_)),
         assertz(counter(Counter)),
         get(Present_C,[spec],Value),!,
         notmember(C,Value),!,
         first_element_defn(Value,FirstElement),
         counter(Counter),
         New_counter is Counter + 1,
         h114_it(FirstElement,C,New_counter,[FirstElement|CTrail]).
         
h114_ita(Present_C,C,Counter,CTrail):-
         get(Present_C,[spec],Value),
         member(C,Value),!,
         rid_ex_cons(C,[Present_C|CTrail]).

rid_ex_cons(C,Ctrail):-
         merge_cons(C,Ctrail),
         retract(C,[worth],[Worth2]),
         Worth3 is Worth2 + Worth2/2,
         assertz(C,[worth],[Worth3]).

merge_cons(C,[]).

merge_cons(C,[C2|Ctrail]):-
         get(C2,X,Y),
         get(C,X,Y),
         retract(C2,X,Y),
         merge_cons(C,Ctrail).
merge_cons(C,[C2|Ctrail]):-
         get(C2,X,Y),
         not(get(C,X,Y)),
         assertz(frame(C,X,Y)),
         retract(frame(C2,X,Y)),
         merge_cons(C,Ctrail).
         
         

h28(C1):-
   get(C,[examples,P],Value),
   get(C1,[examples,P],Value2),
   set_equal_defn(Value,Value2),
   rid_ex_cons(C,[C1]).
h28(C1):-
   h114(C1).
h28(C1):-
   h114a(C1).


h36(C):-
  h36_limited(C,0).

h36_limited(C,Counter):-
   Counter < 100,
   get(C1,[examples,typ],V),
   first_element(V,F),
   get(C1,[defn,name],[Defn]),
   Defn2 =.. [Defn|F],
   Defn2,
   retract(C,[examples,typ],V2),
   append(V2,F,V3),
   assertz(C,[examples,typ],V3).


frame(gob1,[genl],[gob2,gob3]).
frame(gob1,[sap],[ya]).
frame(gob1,[worth],[100]).
frame(gob3,[sap],[ya]).
frame(gob2,[worth],[100]).
frame(gob3,[genl],[gob4]).
frame(gob4,[genl],[gob5]).
frame(gob5,[genl],[gob1]).
frame(gob5,[sap],[ya]).

bget(_,_,_):-write('bogus definition of bget'),nl,
             write('this is a bug, I fail'),nl,
             write('I am in h0.pl'),nl,!,fail.

