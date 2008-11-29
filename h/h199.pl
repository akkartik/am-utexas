/* H199,H200,H201,H202 combined-p.267--
   When coalescing F(a,b,c,..), whose domain/range is <A,B,C..-> R>,
   a good choice of 2 domain components to coalesce is :
           1) a pair of identically equal ones.To fill in alg. for new
   coalesced version , call on F.alg with 2 args. the same. To fill in
   defn. of new coalesced version , call on F.defn with 2 args the same.
           2) barring that, choose a pair related by specialization 
   (eliminate the more general one).
           3) barring that, choose a pair with a common specialization S
   and replace both by S.
   When filling in worth of a new coalesced version of F,a suitable value 
   is 0.9*(worth of F) + 0.1*(worth of coalesce).                        */

   h199(F) :- get(F,[dom_range],D_r),
              split_last(D_r,D,R),
              h199_coalesce(F,D),
              put(F,[coalesce],true).

   h199(F) :- put(F,[coalesce],nil).


   h199_coalesce(F,D) :- h199_equal(D,F).
   h199_coalesce(F,D) :- h199_related(D,F).
   h199_coalesce(F,D) :- h199_comm_spec(D,F).

   h199_equal([[]|_],_) :- fail.
   h199_equal([H|T],F) :- member(H,T),
                          fremove(F,[dom_range],H).
   h199_equal([H|T],F) :- h199_equal(T,F).

   h199_related(D,F) :- rel_by_gen(D,F).
   h199_related(D,F) :- rel_by_spec(D,F).

   rel_by_gen([[]|_],_) :- fail.
   rel_by_gen([H|T],F) :- get(H,[gen1],Gen),
                          intersection(Gen,T,[C|C1]),
                          nonnull(C),
                          fremove(F,[dom_range],C).
   rel_by_gen([H|T],F) :- rel_by_gen(T,F).

   rel_by_spec([[]|_],_) :- fail.
   rel_by_spec([H|T],F) :- get(H,[spec],Spec),
                           intersection(Spec,T,[C|C1]),
                           nonnull(C),
                           fremove(F,[dom_range],H).
   rel_by_spec([H|T],F) :- rel_by_spec(T,F).

   h199_comm_spec([[]|_],_) :- fail.
   h199_comm_spec([H|T],F) :- 
                           get(H,[spec],Spec),
                           common(Spec,T,H1,X),
                           fremove(F,[dom_range],H),
                           fremove(F,[dom_range],H1),
                           put(F,[dom_range],X).

   h199_comm_spec([H|T],F) :- h199_comm_spec(T,F).

   common(L1,[[]|_],_,_) :- fail.
   common(L1,[H|L2],H,C) :- get(H,[spec],Spec),
                            intersection(L1,Spec,[C|C1]),
                            nonnull(C).
   common(L1,[H|L2],H,C) :- common(L1,L2,H1,X).

/* End of H199 */

