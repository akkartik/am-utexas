/* To make sure this list of dynamic definitions is complete:
	1. Erase this list
	2. Consult the buffer of definitions
	3. Cut out the list of prolog's messages from the prolog windowb
	4. Replace all occurrences of "[consulting procedure" with ":-dynamic"
	5. Replace all occurrences of "]" with "."
   and presto! all your defn's are dynamic.   -Todd  */

:-dynamic(seed/1).
:-dynamic(do_threshold/1).

:- dynamic(basecase/0).
:- dynamic(notmember/2).
:- dynamic(myvar/1).
:- dynamic(makedif/2).
:- dynamic(makedif1/3).
:- dynamic(anything_defn/1).
:- dynamic(any_concept_defn/1).
:- dynamic(atom_defn/1).
:- dynamic(object_equality_defn/2).
:- dynamic(normalize/2).
:- dynamic(asort/2).
:- dynamic(ainsert/3).
:- dynamic(delete_alg/3).
:- dynamic(delete_defn/3).
:- dynamic(member_alg/2).
:- dynamic(member_defn/2).
:- dynamic(set_member_defn/2).
:- dynamic(set_member_alg/2).
:- dynamic(length_defn/2).
:- dynamic(length_alg/2).
:- dynamic(set_defn/1).
:- dynamic(set_alg/1).
:- dynamic(insert_defn/3).
:- dynamic(insert_defn2/3).
:- dynamic(set_insert_defn/3).
:- dynamic(set_insert_alg/3).
:- dynamic(set_delete_defn/3).
:- dynamic(set_equal_defn/2).
:- dynamic(set_delete_alg/3).
:- dynamic(bag_defn/1).
:- dynamic(bag_alg/1).
:- dynamic(bag_member_defn/2).
:- dynamic(bag_insert_defn/3).
:- dynamic(bag_equal_defn/2).
:- dynamic(bag_delete_defn/3).
:- dynamic(compose_defn/2).
:- dynamic(getValues/4).
:- dynamic(runF1values/2).
:- dynamic(getDom/2).
:- dynamic(struct_defn/1).
:- dynamic(coalesce_defn/2).
:- dynamic(compare_dR/2).
:- dynamic(compare1_dR/2).
:- dynamic(itdR/4).
:- dynamic(equality_defn/2).
:- dynamic(first_element_defn/2).
:- dynamic(last_element_defn/2).
:- dynamic(rest_defn/2).
:- dynamic(bag_diff_defn/3).
:- dynamic(bag_intersect_defn/3).
:- dynamic(bag_union_defn/3).
:- dynamic(constant_pred_defn/1).
:- dynamic(check_true/1).
:- dynamic(check_false/1).
:- dynamic(constant_h_1/1).
:- dynamic(constant_h_2/1).
:- dynamic(constant_true_defn/1).
:- dynamic(constant_false_defn/1).
:- dynamic(difference_defn/3).
:- dynamic(empty_struct_defn/1).
:- dynamic(nonempty_struct_defn/1).
:- dynamic(intersect_defn/3).
:- dynamic(list_intersect_defn/3).
:- dynamic(list_diff_defn/3).
:- dynamic(list_delete_defn/3).
:- dynamic(list_union_defn/3).
:- dynamic(list_defn/1).
:- dynamic(ordered_pairs_defn/1).
:- dynamic(predicate_defn/1).
:- dynamic(check_range/2).
:- dynamic(list_insert_defn/3).
:- dynamic(set_diff_defn/3).
:- dynamic(oset_diff_defn/3).
:- dynamic(oset_defn/1).
:- dynamic(identity_defn/2).
:- dynamic(object_defn/1).
:- dynamic(reverse_ord_pair_defn/2).
:- dynamic(invert_an_op_defn/2).
:- dynamic(set_intersect_defn/3).
:- dynamic(set_union_defn/3).
:- dynamic(struct_of_struct_defn/1).

/* I am going to change all of these definitions into (Pure) Pure Prolog so
that they will run backwards as well as forwards. This way I can easily 
write functions that are invertable. Motivation: generate examples, make 
inverses easy...If they are not (Pure) Pure enough, oh ye who come after,
rewrite to your hearts content. I think that this should be made a priority
for you. 

I will also take it upon myself to add in all concepts defs of known
concepts with no def. 

One problem is that most of these will not work on one parameter.
Some don't really run true to form backwards and forwards but they
come very close.  At least they don't fail but come back with
something. this problem prompted me to write my general inverse
function the way I did.   -marcos
*/

/*This stuff in the parens passes the test in a very nice way! -marcos*/
basecase.

notmember(a,[]).
notmember(X,[]).
notmember(X,L) :- myvar(X),makedif(X,L),!.
notmember(X,[H|T]) :- \+ X = H, notmember(X,T).


myvar(X) :- var(X).
myvar(X) :- nonvar(X),X=[H|T],myvar(H),myvar(T).


/* makedif(-X,+L) binds X to an atom not in L */

makedif(X,L) :- makedif1(X,L,a),!.

makedif1(X,[],X).
makedif1(X,[Seed|T],Seed) :- randomelement([a,b,c,d,e,f,g],C),
        makename(Seed,C,Newseed),
        makedif1(X,T,Newseed).
makedif1(X,[H|T],S) :- makedif1(X,T,S).

/*added -marcos*/
anything_defn(X).

any_concept_defn(C) :- concept(C).
atom_defn(X) :- atom(X).

%struct_defn(X).
%this is bogus!

/*end stuff!*/

/*This is a text predicate that can does not need to run forwards
and/or backwards but I have modified it so that it may, even with
the cuts in it. -marcos
*/
object_equality_defn(X,Y) :- 
        normalize(X,X1),
        normalize(Y,Y1),!, 
	X1=Y1.

normalize(X,P):- var(X), X = P.
normalize([],[]).
normalize(A,A) :- atomic(A).
normalize(X,[H1|T1]) :-
        asort(X,[H|T]),!,
        normalize(H,H1),!,normalize(T,T1).

asort([],[]).
asort([H|T],S) :-
        asort(T,S1),
        ainsert(H,S1,S).
ainsert(A,[],[A]).
ainsert(A,[H|T],[A,H|T]) :- A @< H.
ainsert(A,[H|T],[H|R]) :- ainsert(A,T,R).


/*end stuff2!*/

/*This next group runs backwards, forwards and sideways*/

delete_alg(A,B,C):-delete_defn(A,B,C).

delete_defn(A,[A|B],B).
delete_defn(A,[B|C],[B|C1]):-delete_defn(A,C,C1).

/*end stuff2*/

/*now this next set will work and return [] as part of the set*/

member_alg(S,E):-member_defn(S,E).

member_defn([],[]).
member_defn(X,[X|T]).
member_defn(X,[_|T]):-member_defn(X,T).

/* not sure about this next one yet*/

set_member_defn([H|_],Element) :- basecase,
        set_equal_defn(Element,H).
set_member_defn([_|T],Element) :- set_member_defn(T,Element).


set_member_alg([H|_],Element) :- basecase,
        set_equal_defn(Element,H).
set_member_alg([_|T],Element) :- set_member_alg(T,Element).

/*length works backwards and forwards*/

length_defn([],[]) :- basecase.
length_defn([_|R],[_|L]) :- length_defn(R,L).

length_alg(X,Y) :- length_defn(X,Y).


set_defn([]):-basecase.
set_defn([H|T]):- notmember(H,T), set_defn(T).
/*note ^^^ notmember(H,T) will instantiate H so that it is not a member of T*/


set_alg(X) :- set_defn(X).

/*added not quite running backwards yet so I'll just call
delete to get the proper inversion. Stupid but it'll work!  
This works with backwards and forwards and only with one
input param! -marcos*/

insert_defn(A,B,C):-var(B),!,delete_defn(A,C,B).
insert_defn(A,B,C):-var(A),var(C),!,randomelement([a,b,c,d,e,f,g,C],A),
	insert_defn(A,B,C).
insert_defn(A,B,C):- normalize(B,B1),normalize(C,C1), 
	insert_defn2(A,B1,C1).
insert_defn2(X,[],[X]).
insert_defn2(X,[],[]).
insert_defn2(X,[A|B],[A|C]):-insert_defn2(X,B,C).
insert_defn2(X,[A|B],[X|C]).


/* set_insert works both ways, almost*/


set_insert_defn(I,Set,Set) :- member_defn(I,Set).
set_insert_defn(I,Set,[I|Set]).

set_insert_alg(I,Set,Set) :- member_alg(I,Set).
set_insert_alg(I,Set,[I|Set]).

/*set delete does not work! Set_equal checks to see if the items are
in a set, strange. What if they are atoms? Is set delete only supposed
to work on deleting sets? Under that assumption it works both ways*/

set_delete_defn(_,[],[]) :- basecase.
set_delete_defn(I,[H|T],T) :-
%	not(equality_defn(I,[])),
        equality_defn(I,H).
set_delete_defn(I,[H|T],[H|T1]) :-
        set_delete_defn(I,T,T1).

set_equal_defn(A,B) :- set_defn(A),set_defn(B),
        object_equality_defn(A,B).

set_delete_alg(I,With,Without) :- 
        set_delete_defn(I,With,Without).

bag_defn([]) :- basecase.
bag_defn([H|T]):- bag_defn(T).

bag_alg(X) :- bag_defn(X).
        
bag_member_defn([],[]) :- basecase.
bag_member_defn([H|_],Element) :- 
        bag_equal_defn(Element,H).
bag_member_defn([_|T],Element) :- bag_member_defn(T,Element).

bag_insert_defn(Object,Bag,[Object|Bag]).
 
bag_equal_defn(A,B) :- bag_defn(A),bag_defn(B),object_equality_defn(A,B).


bag_delete_defn(_,[],[]) :- basecase.
bag_delete_defn(I,[H|T],T) :- object_equality_defn(I,H).
bag_delete_defn(I,[H|T],[H|T1]) :-
        bag_delete_defn(I,T,T1).
        
% Needs to be written (but not by me!)
% Remember this is a defn or an alg -marcos
%from here on out new defn's -marcos
compose_defn(FunList,FG):-
	get(FG,[defn,name],P),
	getarity(P,N),
        getValues(FunList,F,[],ReturnValue),
	append(ReturnValue,X,ArgList),
	Foo  =.. [F|ArgList],
	Foo1 =.. [P|ArgList],!,
        Foo,Foo1.

getValues([F|[]],F,P,P).
getValues([Fun|FunList],F,[P|List],ReturnValue):-
	get(Fun,[defn,name],F1),
	get(Fun,[dom_range],Examples),
       	runF1values(Examples,Dom),
	append(List,Dom,List2),
	getValues(FunList,[P,List2],ReturnValue).

runF1values([],_):-!,fail.
runF1values([E1|Rest],Dom):-
	getDom(E1,Dom).
runF1values([E1|Rest],Dom):-
	runF1values(Rest,Dom).

getDom([],_):-fail,!.
getDom([Dom|[]],Dom):-!.
getDom([_|Rest],Dom):-
	getDom(Rest,Dom).
	
struct_defn(X):- 
	bag_defn(X).

coalesce_defn(F,G):-
	get(F,[dom_range],FDOMRANG),
	get(G,[dom_range],GDOMRANG),
	compare_dR(FDOMRANG,GDOMRANG).

compare_dR([],_).
%I don't know if the next clause is right but here goes:
compare_dR([FDR1|FDOMRANG],GDOMRANG):-
	equality_defn(GDOMRANG,[]),!,
	fail.

compare_dR([FDR1|FDOMRANG],GDOMRANG):-
	compare1_dR(FDR1,GDOMRANG),
	compare_dR(FDOMRANG,GDOMRANG).
compare1_dR(FDR1,[]).
compare1_dR(FDR1,[GDR1|GDOMRANG]):-
	length(FDR1,N),
	N1 is N - 1,
	length(GDR1,N1),
	itdR(FDR1,GDR1,N1,0).

compare1_dR(FDR1,[GDR1|GDOMRANG]):-
	compare1_dR(FDR1,GDOMRANG).
%this function must be fixed to reflect that two arguments of F must
%be the same.
itdR([A|[]],[A|[]],N,N).
itdR([A|Rest],[A|Rest2],N,N2):-
	N3 is N2 + 1,
	itdR(Rest,Rest2,N,N3).
itdR([_|Rest],Rest2,N,N2):-
	N3 is N2 + 1,
	itdR(Rest,Rest2,N,N3).

%trying to keep stuff out of the heads
equality_defn(Z,X):-
	object_equality_defn(Z,X).
equality_defn(Z,Z):- not(object_defn(Z)).
%I am worried that what is needed isn't equal but the various subtypes
%or equality like object_equality, list_equality, bag_equality,etc.
%Lenat seems t be using this sort of equal in his algorithm's but
%I am not sure. He never says. I am assuming that '=' is lisp equal
%the same as the above definition. -marcos
/*  There is another version after this comment, pick your poison...
not(X):- X, !, fail.
not(X).

first_element_defn(A,X):-
	reverse(A,Aii),
        member_defn(Z,Aii),
	delete_defn(Z,A,A2),
	empty_struct_defn(A2),
	equality_defn(Z,X).
first_element_defn(A,X):-
        reverse(A,Aii),
        member_defn(Z,Aii),
	delete_defn(Z,A,A2),
	first_element_defn(A2,X).

last_element_defn(A,X):-
        member_defn(Z,A),
	delete_defn(Z,A,A2),
	empty_struct_defn(A2),
	equality_defn(Z,X).
last_element_defn(A,X):-
        member_defn(Z,A),
	delete_defn(Z,A,A2),
	last_element_defn(A2,X).

*/
	
first_element_defn(A,X):-
	last_element_defn(A,Z),
	delete_defn(Z,A,A2),
	empty_structure_defn(A2),
	equality_defn(Z,X).
first_element_defn(A,X):-
	last_element_defn(A,Z),
	delete_defn(Z,A,A2),
	first_element_defn(A2,X).

last_element_defn(A,X):-
	first_element_defn(A,Z),
	delete_defn(Z,A,A2),
	empty_structure_defn(A2),
	equality_defn(Z,X).
last_element_defn(A,X):-
	first_element_defn(A,Z),
	delete_defn(Z,A,A2),
	last_element_defn(A2,X).

rest_defn([_|B],B).

	
bag_diff_defn(X,Y,Z):- equality_defn(X,[]),equality_defn(Z,[]).
bag_diff_defn(X,Y,Z):- 
	first_element_defn(X,A),
	member_defn(A,Y),
	rest_defn(X,RX),
	bag_delete(A,Y,Y2),
        bag_diff_defn(RX,Y2,Z).
bag_diff_defn(X,Y,Z):-
	first_element_defn(X,A),
	first_element_defn(Z,A),
	rest_defn(X,XB),
	rest_defn(Z,ZB),
	bag_diff_defn(XB,Y,ZB).

bag_intersect_defn([],_,[]).
bag_intersect_defn([A|AR],B,C):-
	member_defn(A,B),
	member_defn(A,C),
	bag_delete_defn(A,B,B2),
	bag_delete_defn(A,C,C2),
	bag_intersect_defn(AR,B2,C2).
bag_intersect_defn([_|AR],B,C):-
	bag_intersect_defn(AR,B,C).

bag_union_defn(A,B,C):-
	equality_defn(A,[]),!,
	bag_equal_defn(B,C).
bag_union_defn(A,B,C):-
	first_element_defn(A,A1),
	bag_delete_defn(A1,A,A2),
	bag_delete_defn(A1,B,B2),
	bag_delete_defn(A1,C,C2),
	bag_union_defn(A2,B2,C2). 

%I'm stuck on the isa predicate part right now!
%canonize_defn(P1,P2,F):-
	
%not sure about what the defn  of conjecture is
%conjecture_defn(X):-

constant_pred_defn(X):- 
	X,!,
	check_true(X).
constant_pred_defn(X):-
	!,
	check_false(X).
check_true(X):-
	!,
	X.
check_false(X):-
	!,
	not(X).

constant_h_1(X):-
	not(X),!,
	not(X).
constant_h_1(X):-!,fail.

constant_h_2(X):-
	X,!,
	X.
constant_h_2(X):-
	!,fail.

constant_true_defn(X):-!.
constant_false_defn(X):-!,fail.


difference_defn(A,B,C):-
	first_element_defn(A,X),!,
	not(member_defn(X,B)),
	member_defn(X,C).

empty_struct_defn(X):-
	struct_defn(X),!,
	equality_defn(X,[]).

nonempty_struct_defn(X):-
	struct_defn(X),
	equality_defn(X,[]),!,fail.
nonempty_struct_defn(X):-basecase.

intersect_defn(A,B,C):-
	list_intersect_defn(A,B,C);
	bag_intersect_defn(A,B,C).

/*a main problem i have with Lenat's algorithms in his thesis is that
he has lots of trouble distinguishing from defn's that are predicates
and defn's that are constructive. IN prolog the distinction is moot.
I think that something should be done with that. -marcos
*/

list_intersect_defn(A,B,C):-
	equality_defn(A,[]),
	equality_defn(C,[]).
list_intersect_defn(A,B,C):-
	first_element_defn(A,A1),
	member_defn(A1,B),
	first_element_defn(C,C1),
	equality_defn(A1,C1),
	list_delete_defn(A1,B,B2),
	rest_defn(C,C2),
	list_intersect_defn(A1,B2,C2).
list_intersect_defn(A,B,C):-
	rest_defn(A,A2),
	list_intersect_defn(A2,B,C).


list_diff_defn(A,B,C):-
	equality_defn(A,[]),
	equality_defn(C,[]).

list_diff_defn(A,B,C):-
	first_element_defn(A,A1),
	equality_defn(A1,B),
	rest_defn(A,A2),
	list_delete_defn(A1,B,B2),
	list_diff_defn(A2,B2,C).

list_diff_defn(A,B,C):-
	first_element_defn(A,A1),
	first_element_defn(C,C1),
	equality_defn(A1,C1),
/* I have left out the obvious optimization: first_element_defn(C,A1),
so that the code with be apparent to AM. -marcos
*/
	rest_defn(A,A2),
	rest_defn(C,C2),
	list_diff_defn(A2,B,C2).
	

list_delete_defn(X,A,B):-
	equality_defn(A,[]),
	equality_defn(B,[]).
list_delete_defn(X,A,B):-
	first_element_defn(A,A1),
	equality_defn(A1,X),
	rest_defn(A,A2),
	equality_defn(A2,B).
list_delete_defn(X,A,B):-
	rest_defn(A,A2),
	rest_defn(B,B2),
	list_delete_defn(X,A2,B2).

list_union_defn(A,B,C):-
	equality_defn(A,[]),
	equality_defn(B,C).
list_union_defn(A,B,C):-
	first_element_defn(A,A1),
	first_element_defn(C,C1),
	equality_defn(A1,C1),
	rest_defn(A,A2),
	rest_defn(C,C2),
	list_union_defn(A2,B,C2).
	
list_defn(X):-
	equality_defn(X,[]).
list_defn(X):-
	rest_defn(X,X2),
	list_defn(X2).

%quick def, -marcos
ordered_pairs_defn(X):-
	equality_defn(X,[A,B]).
%slow def, but maybe better for AM-marcos
ordered_pairs_defn(X):-
	list_defun(X),
	not(equality_defn(X,[])),
	member(Z,X),
	list_delete_defn(Z,X,S1),
	not(equality_defn(S1,[])),
	member(Y,S1),
	list_delete_defn(Y,S1,[]).


predicate_defn(X):-
	equality_defn(X,[]),!,
	fail.
	
predicate_defn(X):-
	get(X,[dom_range],DR),
	check_range(DR,[true_false]).

check_range(DR,X):-
	first_element_defn(DR,DR1),
	split_last(DR1,X).	
check_range(DR,X):-
	rest_defn(DR,DR2),
	check_range(DR2,X).

list_insert_defn(X,A,B):-
	first_element_defn(B,B1),
	rest_defn(B,B2),
	equality_defn(B1,X),
	equality_defn(B2,A).

set_diff_defn(A,B,C):-
	equality_defn(A,[]),
	equality_defn(C,[]).
set_diff_defn(A,B,C):-
	first_element_defn(A,A1),
        member_defn(A1,B),
	set_delete_alg(A1,B,B2),
	set_diff_defn(A1,B2,C).
set_diff_defn(A,B,C):-
	first_element_defn(A,A1),
	first_element_defn(C,C1),
	equality_defn(A1,C1),
	rest_defn(A,A2),
	rest_defn(C,C2),
	set_diff_defn(A2,B,C2).
	


oset_diff_defn(A,B,C):-
	equality_defn(A,[]),
	equality_defn(C,[]).
oset_diff_defn(A,B,C):-
	first_element_defn(A,A1),
        member_defn(A1,B),
	oset_delete_alg(A1,B,B2),
	oset_diff_defn(A1,B2,C).
oset_diff_defn(A,B,C):-
	first_element_defn(A,A1),
	first_element_defn(C,C1),
	equality_defn(A1,C1),
	rest_defn(A,A2),
	rest_defn(C,C2),
	oset_diff_defn(A2,B,C2).
	
	

oset_defn(A):-
	equality_defn(A,[]).
oset_defn(A):-
	rest_defn(A,A2),
	oset_defn(A2).

identity_defn(A,B):-
	equality_defn(A,B).

object_defn(X):-
	exs(object,Y),!,
	member(X,Y).

reverse_ord_pair_defn(P,Q):-
	first_element_defn(P,P1),
	last_element_defn(Q,Q2),
	equality_defn(P1,Q2),
	last_element_defn(P,P2),
	first_element_defn(Q,Q1),
	equality_defn(P2,Q1).


/* I don't get these. -marcos
*/
%projection1_defn(A)
%projection2_defn(A)

/*
invert_op_defn(F,G):-
	getarity(G,N),
	getarity(F,N2),
	get(dom_range,F,FDR),
	get(dom_range,G,GDR),
	*/
%for the above I will simply use the heuristic I wrote to do this.
%However, please not that there is a big difference between this
%defn, which produces a product, and the inverted_op defn which
%is a predicate. Is this right? I think that the next defn must be
%redone and alot of evaluation must in injected into it. -marcos

invert_an_op_defn(F,G):- mh9(F), makename('inverse_of_',F,G).
	
%inverted_op(F):- 
/*
parallel_join_defn(S1,F,G):-
	get(G,[dom_rang],DomRange),
	splitlast(DomRange,Range),
	delete

parallel_join2_defn(S1,S2,F,G):-

parallel_replace_defn(S1,F,G):-

parallel_replace2_defn(S1,S2,F,G):-


repeat_defn(S1,F1,G1):-
	first_element_defn(
*/
/*
restrict_defn(F,G):-
	not(var(G)),
	get(F,[dom_range],DRF),
	get(G,[dom_range],DRG),
	compare_for_restrict(DRF,DRG),
	compare_defns(F,G).

restrict_defn(F,G):-
	var(G),
	get(F,[dom_range],DRF),
	subvarsforDR(DRF,DRG),     
	G1 = frame(G,[dom_range],DRG).
	assert(G1),
	compare_for_restrict(DRF,DRG),
	compare_defns(F,G).

subvarsforDR([],[]).
subvarsforDR([X|RestDRF],[_|RestDRG]):-
	subvarsforDR(RestDRF,RestDRG).
*/

set_intersect_defn(A,B,C):-
	equality_defn(A,[]),
	equality_defn(C,[]).
set_intersect_defn(A,B,C):-
	member_defn(Z,A),
	not(equality_defn(Z,[])),
	member_defn(Z,B),
	member_defn(Z,C),
	set_delete_defn(Z,A,A1),
	set_delete_defn(Z,B,B1),
	set_delete_defn(Z,C,C1),
	set_intersect_defn(A1,B1,C1).
	
set_intersect_defn(A,B,C):-
	member_defn(Z,A),
	set_delete_defn(Z,A,A1),
	set_intersect_defn(A1,B,C).

set_union_defn(A,B,C):-
	equality_defn(A,[]),
	equality_defn(B,C).

set_union_defn(A,B,C):-
	first_element(A,A1),
	member_defn(A1,C),
        rest_defn(A,A2),
	set_delete_defn(A1,B,B2),
	set_delete_defn(A1,C,C2),
	set_union_defn(A2,B2,C2).

struct_of_struct_defn(S):-
	empty_struct_defn(S).
struct_of_struct_defn(S):-
	struct_defn(S),
	member(Z,S),
	struct_defn(Z),
	delete_defn(Z,S,S2),
	struct_of_struct(S2).

	
