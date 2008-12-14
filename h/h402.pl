/* Some of these heuristics were derived from Lenat'st thesis. They were
   not explicitly listed though. They a start at 402.


*/ 

/* if the number of examples of C are between 5 and 30 increment worth.
   This is Lenat's no too many not too few */


h402(C):- 
	(exs(C,Examples);get(C,[examples,typ],Examples)),
        length(Examples,N),
        N > 5, N < 30,
        (retract(frame(C,[worth],[Worth]));Worth = 100),
        Worth2 is Worth + (Worth + 1)/5,
        assertz(frame(C,[worth],[Worth2])).

h402(C):- 
	(exs(C,Examples);get(C,[examples,typ],Examples)),
        length(Examples,N),
        N < 5,
        (retract(frame(C,[worth],[Worth]));Worth = 1),
        Worth2 is Worth/2,
        assertz(frame(C,[worth],[Worth2])).



h402(C):- 
	(exs(C,Examples);get(C,[examples,typ],Examples)),
        length(Examples,N),
        N > 40,
        (retract(frame(C,[worth],[Worth])); Worth = 1),
        Worth2 is Worth/2,
        assertz(frame(C,[worth],[Worth2])).


bget(_,_,_):-write('bogus definition of bget'),nl,
             write('this is a bug, I fail'),nl,
             write('I am in h402.pl'),nl,!,fail.

