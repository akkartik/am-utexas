/* H204- p.268- If an active concept F(x,y) takes a pair of N's as
   arguments, then create a new concept , a spec of F, F_itself,
   taking one N as argument defined F(x,x) with initial worth
   worth(F). If AM has never coalesced before ,this gets bonus value.
   If AM has coalesced F before into S, modify this suggs's value 
   according to current worth of S.                                   */

   h204(C) :- get(C,[dom_range],D_r),
              member([D1,D1,R],D_r),
              makename(C,'_itself',Newname),
              gensym(Newname,N),
              makename(N,'_alg',Alg),
              makename(N,'_defn',Defn),
              h204assert(Defn,C),
              h204assert(Alg,C),
              put(N,[defn,name],Defn),
              put(N,[alg],Alg),
              put(N,[dom_range],[D1,R]),
              put(N,[name],N),
              h204_worth(N,C),
              put(coalesce,[examples,typ],N),
              put(C,[spec],N),
              put(C,[coalesce],N),
              addtoagenda(fillin,N,[examples,typ],200,'No examples of concept exist').

h204assert(Name,Alg) :-
        Body =.. [Alg,X,X],
        Head =.. [Name,X],
        assertz((Head :- Body)).

   h204_worth(N,C) :- get(C,[coalesce],[]),
                      get(C,[worth],W),
                      W1 is W+100,
                      put(N,[worth],W1).

   h204_worth(N,C) :- get(C,[coalesce],[V|T]),
                      get(V,[worth],W),
                      put(N,[worth],W).


/* End of H204   */
  
