/*************************************************************
 * h59c anyconcept.examples.check
 * Prune the exs slot of a concept to a size reflecting its worth
 * For the moment this may be considered to be its worth/20,
 * So a concept of worth 800 would have up to 40 exs.
 */

h59c(C) :-
        get(C,[examples,typ],Exs),
        get(C,[worth],[W]),
        compute_number_to_remove(Exs,W,Num),
        prune_to_size(Exs,Num,Newexs),
        update(C,[examples,typ],Newexs).

prune_to_size(A,N,A) :- N =< 0.
prune_to_size(E,N,NewE) :-
        remove_random(E,NewE1),
        N1 is N-1,
        prune_to_size(NewE1,N1,NewE).

/***
**** we will allow at least 15 exs for any concept,and up to worth/20.
**** comput_number_to_remove will fail if there are none to remove.
***/

compute_number_to_remove(Exs,W,Num) :-
        length(Exs,L),
        Allow is W/20,
        ((Allow < 15,N=15) ; N = Allow),
        L > N,
        Num is L - N.

