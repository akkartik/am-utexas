/* H116- p.250- To fill in In_dom_of of concept X, ripple down the tree
   of concepts starting at Active to determine which active concepts can
   be run on X's.                                                    */


   h116(C) :- specs_sf(activity,Ops),
              h116_get_list(Ops,C,List),
              putvals(C,[in_dom_of],List).

   h116_get_list([],_,[]).
   h116_get_list([O|Ops],C,L) :- 
        get(O,[dom_range],[]),
        addtoagenda(fillin,O,[dom_range],200,'no value for this slot'),
        h116_get_list(Ops,C,L).
   h116_get_list([O|Ops],C,[O|L]) :-
              get(O,[dom_range],D_r),
              split_last_all(D_r,D,R),
              flatten(D,D1),
              member(C,D1),
              h116_get_list(Ops,C,L).

   h116_get_list([O|Ops],C,List) :-
              h116_get_list(Ops,C,List).

split_last_all([],[],[]).
split_last_all([H|T],[HH|TH],[HT|TT]) :-
        split_last(H,HH,HT),
        split_last_all(T,TH,TT).

/* End of H116 */
