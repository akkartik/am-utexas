h114a(C):-h114_ita(C,C,0,[]).

h114_it(Present_C,C,Counter,CTrail):-
         Counter < 100,
         retract(counter(_)),
         assertz(counter(Counter)),
         get(Present_C,[genl],Value),!,
         notmember(C,Value),!,
         first_element_defn(Value,FirstElement),
         counter(Counter),
         New_counter is Counter + 1,
         h114_it(FirstElement,C,New_counter,[FirstElement|CTrail]).
         
h114_it(Present_C,C,Counter,CTrail):-
         get(Present_C,[genl],Value),
         member(C,Value),!,
         rid_ex_cons(C,[Present_C|CTrail]).

h114_ita(Present_C,C,Counter,CTrail):-
         Counter < 100,
         retract(counter(_)),
         assertz(counter(Counter)),
         get(Present_C,[spec],Value),!,
         notmember(C,Value),!,
         first_element_defn(Value,FirstElement),
         counter(Counter),
         New_counter is Counter + 1,
         h114_it(FirstElement,C,New_counter,[FirstElement|CTrail]).
         
h114_ita(Present_C,C,Counter,CTrail):-
         get(Present_C,[spec],Value),
         member(C,Value),!,
         rid_ex_cons(C,[Present_C|CTrail]).

rid_ex_cons(C,Ctrail):-
         merge_cons(C,Ctrail),
         retract(C,[worth],[Worth2]),
         Worth3 is Worth2 + Worth2/2,
         assertz(C,[worth],[Worth3]).

merge_cons(C,[]).

merge_cons(C,[C2|Ctrail]):-
         get(C2,X,Y),
         get(C,X,Y),
         retract(C2,X,Y),
         merge_cons(C,Ctrail).
merge_cons(C,[C2|Ctrail]):-
         get(C2,X,Y),
         not(get(C,X,Y)),
         assertz(frame(C,X,Y)),
         retract(frame(C2,X,Y)),
         merge_cons(C,Ctrail).
         
         
