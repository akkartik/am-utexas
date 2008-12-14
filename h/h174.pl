:-public(h174/2).

/* h174 creates a new operation by composing two existing operations.In clausal
   form, the composition is expressed as fog(a,b,c,y):-g(a,b,c,x),f(x,y) which
   is equivalent to f(g(a,b,c)) in functional notation. First function f is
   checked to make sure that it has "arity" 2 (one input and one output 
   parameter). Next the lists of valid domains and ranges of the two functions
   are checked to find a valid composite domain-range. Then the new algorithm
   is created as a single prolog clause and is asserted. Finally a new concept
   frame is filled in. Note that this heuristic is not really very useful
   given the operations that we have defined; this is because very few of
   our operations are of arity 2 with one input and one output parameter. */

/* I noticed that all the other heuristics are of arity 1 and compute on
   a single concept. I have also noticed that this heuristic never seems
   to succeed and I hope that this will give it a better change to succeed.
   -marcos

Ray's original code is in the rile h174-old.pl

*/

h174(F) :- 
        getarity(F,2),
        get(G,[dom_range],Gdr),
        h174(F,G).


h174(F,G):-
        getarity(F,2),
        get(F,[dom_range],Fdr),
        get(G,[dom_range],Gdr),
        get_composite_dr(Fdr,Gdr,FoGdr),
        make_composite_alg(F,G,FoGdr,Newname,Alg),
        create_composite_concept(F,G,Newname,Alg,FoGdr).

/* Since I took ray's comment (see above) to mean that F should be allowed
   to have arity 2+, I am defining a compose function that will allow
   F to have arity n. I hope this works. 

   We start with the same h174, and then add to it a  that will look for
   a set of concepts to fill up the arity of F -marcos

   Basically this is the way the compose function works. Given 2 functions
   as input F(A...N) and G(A..N) compose creates a composition that composes
   G with F and fills in the rest of Fs slots with compatable functions:

   Given the functions F(A,B,C),G(D,E,F),H(G,I,K), this function creates
   the composition F(G(D,E,F),H(G,I,K)) (C is the prolog output parameter).

   This is way it looks like in PROLOG:

   FoGH(A,B,D,E,Y):- G(A,B,X), IntermedFoH(D,E,X,Y).
   IntermedFoH(D,E,X,Y):- H(D,E,Q), IntermedF(Q,X,Y).
   IntermedF(Q,X,Y):- F(Q,X,Y).

   It is written in a general way that will allow F to be arity N -marcos*/
   

h174(F,G):-
        assertz(flag),
        getarity(F,N1), N is N1 -1,
        loop_composit(F,G,N,[],Glist,[],FoGdr,1),
        makename(F,'_o_',Temp),
        loopmakename(Temp,Glist,SeedName),
        loop_make_composit(F,SeedName,Glist,FoGdr,Newname,Algorogo,N,0),!,
        assertz(flag),
        create_composite_concept2(F,Glist,Newname,Algorogo,FoGdr).        
        

loopmakename(Temp,[],Temp).
loopmakename(Temp,[G|Glist],R):-
	makename(Temp,G,Iname),
	loopmakename(Iname,Glist,R).


loop_make_composit(F,Topname,[],[],Newname2,Alg2,Arity,Q):-
	getarity(F,N),
	makelist(N,List),
        get(F,[defn,name],[Name]),
	Pred1 =.. [Name|List],
        Pred2 =.. [Topname|List],
        Alg = (Pred2:-Pred1),
write(Alg),
        assertz(Alg).
	
loop_make_composit(F,Topname,[G|Glist],[FoGdr|FoGdrlist],Newname2,Alg2,Arity,Q):-
        length(FoGdr,N),
        N1 is N + 1,
        makelist(N1,[Frange,Grange|Gdom]),
        append(Gdom,[Grange],T1),
        sumup(FoGdrlist,0,N2),
        write('Q is '),write(Q),nl,
        write('N2 is '),write(N2),nl,
        N3 is N2 + Q,
        makename(G,'_Caller',Temp),		
        makename(Temp,Arity,Iname),
        makelist(N3,Hdomain),
        append(Hdomain,[Grange],Hdomain1),
        append(Hdomain1,[Frange],Hdomain2),
        append(Gdom,Hdomain,Newdr1),
        append(Newdr1,[Frange], Newdr),
        get(G,[defn,name],[Main_functor_G]),
        Func1 =.. [Main_functor_G|T1],
        Func2 =.. [Iname|Hdomain2],
        Func3 =.. [Topname|Newdr],
        Alg=(Func3:-Func1,Func2),!,
%debugging
write(Alg),
        (/*flag,*/fail,
        check_with_user2(F,G,Topname,Alg,Newname2),
        assertz(newname(Newname2));
        Newname2 = Topname),
        Func4=..[Newname2|Newdr],
        Alg2=(Func4:-Func1,Func2),
        assertz(Alg2),
        (retract(flag); true),
        Arity2 is Arity - 1,
        Q2 is Q + 1,
        loop_make_composit(F,Iname,Glist,FoGdrlist,_,_,Arity2,Q2).
        
sumup([],N2,N2).
sumup([L1|List],N2,N3):-
	length(L1,N4),
	N5 is N2 + N4 - 1,
	sumup(List,N5,N3).
        
loop_composit(F,G,N,X,X,Y,Y,_):-
        N = 0.

loop_composit(F,G,N,Glist,New_Glist,Old_FoGdr,New_FoGdr,It) :-
        get(F,[dom_range],Fdr),
        bget(G,[dom_range],Gdr),
        get_composite_dr(Fdr,Gdr,FoGdr,It),
        N1 is N - 1,
        It2 is It + 1,
        loop_composit(F,_,N1,[G|Glist],New_Glist,[FoGdr|Old_FoGdr],
		      New_FoGdr,It2).
      
/* a general get_composit_dr */
get_composite_dr(Fdr,Gdr,FoGdr,It):-
        member(X,Gdr),		
        split_last(X,Gdom,Grange),
        genls_sf(Grange,Gens_of_grange),
        member(FDR,Fdr),
	split_last(FDR,Fdom,Frange),
	get_arg_num_val(Fdom,Fdom2,It,1),
        member(Fdom2,Gens_of_grange),
        append(Gdom,[Frange],FoGdr).


get_arg_num_val([DOM1|DOMLIST],DOM1,IT,IT).

get_arg_num_val([_|DOMLIST],DOM1,IT,IT3):-
        IT2 is IT3 + 1,
	get_arg_num_val(DOMLIST,DOM1,IT,IT2).
 	
        



/* get_composite_dr finds a legal domain-range for the composition. It searches
   the domain-range lists of f and g for a pair such that the
   range of g is the domain of f. The legal composite is g's domain and f's
   range. */
get_composite_dr(Fdr,Gdr,FoGdr):-
        member(X,Gdr),		
        split_last(X,Gdom,Grange),
        genls_sf(Grange,Gens_of_grange),
        member([Fdom,Frange],Fdr),
        member(Fdom,Gens_of_grange),
        append(Gdom,[Frange],FoGdr).

/* make_composite_alg creates f o g in clausal form. First it gensyms a list
   of symbols to serve as variables in the clause. Then it assigns them to
   g's domain, g's range(1), and f's range(1). Then a new clause is synthesized
   and given the tentative name F_o_G (where F_o_G(...Y):-G(...X),F(X,Y).).
   Finally the user is shown the composition and is given a chance to reject it
   or to rename it, then the algorithm is asserted. */
make_composite_alg(F,G,FoGdr,Newname2,Alg):-
        length(FoGdr,N),N1 is N+1,
        makelist(N1,[Frange,Grange|Gdom]),
        append(Gdom,[Grange],T1),
        T2=[Grange,Frange],
        get(F,[defn,name],[Main_functor_F]),
        get(G,[defn,name],[Main_functor_G]),
        Func1=..[Main_functor_G|T1],
        Func2=..[Main_functor_F|T2],
        makename(F,'_o_',Temp),
        makename(Temp,G,Newname),
        append(Gdom,[Frange],Newdr),
        Func3=..[Newname|Newdr],
        Alg=(Func3:-Func1,Func2),!,
%debugging
write(Alg),
        check_with_user2(F,G,Newname,Alg,Newname2),
        Func4=..[Newname2|Newdr],
        Alg2=(Func4:-Func1,Func2),
        assertz(Alg2).

/* create_composite_concept creates a concept frame for the new operation.
   Then a task is added to the agenda to generate examples of the new
   concept. */
create_composite_concept(F,G,Conceptname,Alg,Dom_range):-
        put(Conceptname,[name],Conceptname),
        put(Conceptname,[defn,name],Conceptname),
        put(Conceptname,[alg],Conceptname),
%I had to use putvals and not put as ray had it. That was a bug -marcos
        putvals(Conceptname,[dom_range],Dom_range),
        put(Conceptname,[genl],F),
        put(Conceptname,[genl],G),
        put(Conceptname,[compose],[F,G]), /* pseudo-facet identifies composition
 */
        get(F,[worth],[W1]),
        get(G,[worth],[W2]),
        New_worth is (W1+W2)/2,
        put(Conceptname,[worth],New_worth),
        addtoagenda(fillin,Conceptname,[examples],200,
                    'no examples of this new concept').

create_composite_concept2(F,Glist,Conceptname,Alg,Dom_range):-
        put(Conceptname,[name],Conceptname),
        put(Conceptname,[defn,name],Conceptname),
        put(Conceptname,[alg],Conceptname),
%I had to use putvals and not put as ray had it. That was a bug -marcos
        putvals(Conceptname,[dom_range],Dom_range),
        put(Conceptname,[genl],F),
        putlist(Conceptname,[genl],Glist),
	append([F],Glist,FGlist),
        put(Conceptname,[compose],FGlist), 
/* pseudo-facet identifies composition
 */
        get(F,[worth],[W1]),
        get_ave_worth(Glist,0,0,W2),
        New_worth is (W1+W2)/2,
        put(Conceptname,[worth],New_worth),
        addtoagenda(fillin,Conceptname,[examples],200,
                    'no examples of this new concept').


/*utility-ru for my bastard piece of modification on ray's code.*/
get_ave_worth([],C,W2,R):- R is W2/C.
get_ave_worth(Glist,C,W2,_):- var(Glist), !,fail.
get_ave_worth([G|Glist],C,W2,R):- 
	C2 is C + 1,
        get(G,[worth],[W4]),
        W3 is W2 + W4,
	get_ave_worth(Glist,C2,W3,R).

/*this is just putvals rewritten because I did not know about
putvals -marcos.*/
putlist(Conceptname,FS,[]).
putlist(Conceptname,FS,[G|Glist]):-
	put(Conceptname,FS,G),
	putlist(Conceptname,FS,Glist).

/* check_with_user2 allows the user to discard or rename a newly created 
   composition. If he/she renames the concept, the new name is returned.
   Note that this routine is very similar to check_with_user written by Bruce.
   */
/* taking this unnecessary gabbing out! -marcos*/
check_with_user2(F,G,Newname,Alg,Newname2):-
	      assertz(gensymed_concepts(Newname,Alg,none)).
check_with_user2(F,G,Newname,Alg,Newname2):-
        nl,nl,
        write('I have created a new concept definition which is a composition of '),nl,
        write(F),write(' and '),write(G),nl,
        write('This concept is defined as follows:'),nl,
        write(Alg),nl,
        write('Do you want to keep this new concept (y/n)? '),
        nl,aminput('y'),
        write('Please type new name for this concept or <CR> to keep the current name: '),
        nl,aminput(X),
        ((X='',Newname2=Newname);
         (\+ X='',Newname2=X)).

