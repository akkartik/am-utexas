% The rest of the documentation and code is in h300.pl

h301(Con):-
   time(T),
   T1 is T/2,
   clock(Start,_),  !,
   mutate_defn(Con,spec,T1,Start).
h301(_).

