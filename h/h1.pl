/*******************************************
 *      h1 anything.suggest     
 *      boost worth of recently referenced concepts
 */

h1(_) :-
        history(H),
        recent_cons(5,H,Cons),
        boost_worth(Cons,100).

recent_cons(0,_,[]).
recent_cons(_,[],[]).
recent_cons(N,[H|T],[C|Cons]) :-
        H = [_,C,_,_,_],
        N1 is N - 1,
        recent_cons(N1,T,Cons).

boost_worth([],_).
boost_worth([C|Cons],N) :-
        get(C,[worth],[W]),
        W1 is W + N,
        put(C,[worth],W1),
        boost_worth(Cons,N).
