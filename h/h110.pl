/* H110- p.249- When checking a specialization S of a spec Xof a concept C,
   if there exist other specs of specs of C, then ensure that none are same
   as S.                                                                 */

   h110(C) :- get(C,[spec],Spec),
              h110_get_all_spec(Spec,Spec2),
              h110_two(Spec,Spec2).

   h110_get_all_spec([],[]).
   h110_get_all_spec([C|T],All_spec) :-
              get(C,[spec],Spec),
              h110_get_all_spec(T,Rest),
              union(Spec,Rest,All_spec).
   
      h110_two([],_).
      h110_two([C|Rest],List) :- 
              get(C,[spec],Spec),
              h110_same(Spec,List,Newspec),
              putvals(C,[spec],Newspec),
              h110_two(Rest,List).
  
   h110_same([],_,[]).
   h110_same([S|Spec],List,[S|New]) :-
              h110_run_alg(S,Res1), Val is 0,
              h110_check(Res1,List,Val),
              Val=1,
              h110_same(Spec,List,New).
   h110_same([S|Spec],List,New) :- h110_same(Spec,List,New). 
  
   h110_check(_,_,Val) :- Val>1.
   h110_check(_,[],_).
   h110_check(Res1,[L|List],Val) :-
              h110_run_alg(L,Res2),
              Res1=Res2, Val is Val+1, 
              h110_check(Res1,List,Val).
   h110_check(Res1,[L|List],Val) :-
              h110_check(Res1,List,Val).
   
   h110_run_alg(C,Res) :- 
              examples(C,Ex),
              Ex=[],
              addtoagenda(fillin,C,[examples],200,'No examples of concept').

           h110_run_alg(C,Res) :- 
              examples(C,Ex),
              get(C,[alg],Alg),
              apply(Alg,[Ex|Res]).


/* End of H110  */

