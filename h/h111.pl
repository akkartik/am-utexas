/* H111- p.249- When checking Gen1/Spec of concept C, ensure that C.Gen1
   and C.Spec have no common member Z. If they do ,conjecture that C and Z
   are actually equivalent.                                              */

   h111(C) :- get(C,[gen1],Gen),
              get(C,[spec],Spec),
              intersection(Gen,Spec,Z),
              nonnull(z),
              put(C,[conjec],C=Z).

/* End of H111 */

