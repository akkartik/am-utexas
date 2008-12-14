/* This is Ray's original code:

:-public h174/2.
?-no_style_check(all).





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

h174(_) :- fail.
h174(F,G):-
        getarity(F,2),
        get(F,[dom_range],Fdr),
        get(G,[dom_range],Gdr),
        get_composite_dr(Fdr,Gdr,FoGdr),
        make_composite_alg(F,G,FoGdr,Newname,Alg),
        create_composite_concept(F,G,Newname,Alg,FoGdr).

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
        put(Conceptname,[dom_range],Dom_range),
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

/* check_with_user2 allows the user to discard or rename a newly created 
   composition. If he/she renames the concept, the new name is returned.
   Note that this routine is very similar to check_with_user written by Bruce.
   */
check_with_user2(F,G,Newname,Alg,Newname2):-
        nl,nl,
        write('I have created a new concept definition which is a composition of
'),nl,
        write(F),write(' and '),write(G),nl,
        write('This concept is defined as follows:'),nl,
        write(Alg),nl,
        write('Do you want to keep this new concept (y/n)? '),
        nl,aminput('y'),
        write('Please type new name for this concept or <CR> to keep the current
 name: '),
        nl,aminput(X),
        ((X='',Newname2=Newname);
         (\+ X='',Newname2=X)).



*/
