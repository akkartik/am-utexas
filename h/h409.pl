        
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
