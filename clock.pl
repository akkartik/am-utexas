:- public([clock/2]).

clock(S,_) :- var(S), statistics(runtime,[S1,_]),
        S is S1 / 100, !.
clock(S,T) :-  var(T),nonvar(S),
        statistics(runtime,[T1,_]),
        T2 is T1 / 100,
        T3 is (T2 - S),
        ((T3 = 0, T = 1);
         T = T3),!.

/* this predicate is in this file because it does not seem to 
 compile properly.  Clock must be interpreted as well. */

:- op(100,fx,c).
c(X) :- makename(X,'.pl',Y),compile(Y).

