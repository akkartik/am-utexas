
h36(C):-
  h36_limited(C,0).

h36_limited(C,Counter):-
   Counter < 100,
   get(C1,[examples,typ],V),
   first_element(V,F),
   get(C1,[defn,name],[Defn]),
   Defn2 =.. [Defn|F],
   Defn2,
   retract(C,[examples,typ],V2),
   append(V2,F,V3),
   assert(C,[examples,typ],V3).

