
/* H240- p.275- To fill in some new examples of the structure S,where
   S is a structure admitting multiple occurences of the same element,
   when some examples already exist,-- pick an existing example and randomly 
   change the multiplicity with which various members occur within the 
   structure.                                                             */

   h240(S) :- examples(S,Ex),
              time(T), T1 is T*1/3,
              clock(Start,_),
              h240_do_while_time(T1,Start,Ex,List),
              putvals(S,[examples,typ],List).


   
   h240_do_while_time(Alotment,Start,_,List) :- clock(T), T>Alotment.

   h240_do_while_time(A,St,Ex,[New|List]) :-
              length(Ex,L),
              random(L,N),
              nth(Ex,N,E),
              mult_occ(E,X,N1),
              random(N1,N2),
              N1 is N1-N2,
              del_n_occ(E,X,N1,New),
              h240_do_while_time(A,St,Ex,List).

   mult_occ(L,X,N) :- length(L,Len),
                      random(Len,R),
                      nth(L,R,X), 
                      N is 0,
                      occ(L,X,N).

   occ([],_,_).
   occ([H|T],H,N) :-  N is N+1,
                      occ(T,H,N).
   occ([H|T],X,N) :-  occ(T,X,N).

   del_n_occ(L,X,N,L1) :- 
                      del2(L,X,N,0,L1).
   
   del2(L,X,N,N,[]).
   del2([H|T],H,N,N1,L) :-
                      N1 is N1+1,   del2(T,H,N,N1,L).
   del2([H|T],X,N,N1,[H|L]) :- del2(T,X,N,N1,L).

/* End of H240 */
