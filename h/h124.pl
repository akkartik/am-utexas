h124(I):-nl,write('Sorry, this heuristic is out of order at this time...'),nl.
/* H124- p.252- To fill in domain entry for active concept F, run F
   on various entities, rippling down tree of concepts , to determine
   where F seems to be defined.                                      

   h124(F) :- specs_sf(anything,C),
              get(F,[alg],[Alg]),
              h124_get_dom(C,Alg,List),
              putvals(F,[dom_range],List).

   h124_get_dom([],_,[]).
   h124_get_dom([C|T],Alg,[C|List]) :-
              examples(C,Ex),


              apply(Alg,[Ex|Res]),
              h124_get_dom(T,Alg,List).


   h124_get_dom([C|T],Alg,List) :-
              h124_get_dom(T,Alg,List).

*/

/* End of H124 */

