/* H117 - p.250- To fill in in_ran_of facet of concept X, ripple down
   tree of concepts, starting at Active, to determine which active concepts
   can be run to yield X's.                                            */

   h117(C) :- specs_sf(activity,Ops),
              h117_get_list(Ops,C,List),
              putvals(C,[in_ran_of],List).

h117_get_list([],_,[]).
h117_get_list([O|Ops],C,L) :- 
              get(O,[dom_range],[]),
              addtoagenda(fillin,O,[dom_range],200,'no examples of this slot'),
              h117_get_list(Ops,C,L).

h117_get_list([O|Ops],C,[O|L]) :-
              get(O,[dom_range],D_r),
              split_last_all(D_r,D,R),
              flatten(R,R1),
              member(C,R1),
              h117_get_list(Ops,C,L).

h117_get_list([O|Ops],C,List) :-
              h117_get_list(Ops,C,List).

/* End of H117 */

