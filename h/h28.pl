
h28(C1):-
   get(C,[examples,P],Value),
   get(C1,[examples,P],Value2),
   set_equal_defn(Value,Value2),
   rid_ex_cons(C,[C1]).
h28(C1):-
   h114(C1).
h28(C1):-
   h114a(C1).

