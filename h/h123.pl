:- public(h123/1).
/**********************************************************************
 *
 * h123 finds examples of active concept Con by collecting examples
 * from the domain of Con, running Con.alg, making the example
 * entry [d1,d2,...dn,v] where v is the output of the alg.
 *
 * h123 will use time proportional to the number of examples
 *      found in the domain of CON.
 */

/********************************************************
 *              NEW VERSION -SMARTER
 *  NOTE: this heuristic will not work on predicate because
 *      Predicates do not really have a range.  They should really
 *      return true/false as a third argument.  Until they do,
 *      This heuristic will only work on relations/operations
 */

h123(C) :-
        get(C,[dom_range],[]),
        addtoagenda(fillin,C,[dom_range],200,'No current value for this slot').
h123(C) :-
        get(C,[dom_range],D_r),
        get(C,[alg],[Alg]),
        get_doms(D_r,D),
        getunique_dom(D,D_unique),
        collect_exs(D_unique,D_exs),
        fillin_if_few(D_exs),
        compute_time(D_exs,T),
        do_while_time(T,C,D,D_exs,Alg).
        

/* note that dwt generates at most 25 examples from each d/r pair */

do_while_time(_,C,[],_,_).
do_while_time(Allot,C,[D|R],D_exs,Alg) :-
        clock(Start,_),
        dwt(Start,Allot,C,D,D_exs,Alg),
        clock(Start,T),
        cleanup_a(h123,Num),
        ((Num > 0,
          addtoagenda(check,C,[examples],200,'just found examples of con'),
          put(C,[examples,dif],[Num,T]));
         true),
        do_while_time(Allot,C,R,D_exs,Alg).

dwt(Start,Allot,Con,D,D_exs,Alg) :-
        ok_a(Start,Allot),
        get_rand_args(D,D_exs,Args),
        append(Args,[Val],Ex),
        apply(Alg,Ex),
        put(Con,[examples,typ],Ex),
        addnum_a(h123),
        termination_a(h123,Start,Allot).



get_rand_args([],_,[]) :- !.
get_rand_args([D|DD],D_exs,[A|AA]) :-
        get_rand_args(DD,D_exs,AA),
        member([D,N,L],D_exs),
        random(N,Rand),
        nth(L,Rand,A),!.

get_doms([],[]).
get_doms([H|T],[D|DT]) :-
        split_last(H,D,_),
        get_doms(T,DT).

getunique_dom(D,D_unique) :-
        flatten(D,D_flat),
        removedups(D_flat,D_unique).

collect_exs([],[]).
collect_exs([D|R],[[D,N,Exs]|Rest]) :-
        collect_exs(R,Rest),
        exs(D,Exs),
        length(Exs,N).

fillin_if_few([]).
fillin_if_few([[D,N,_]|R]) :-
        N < 5,
        addtoagenda(fillin,D,[examples],150,'there are few examples of con'),
        fillin_if_few(R).
fillin_if_few([H|T]) :- fillin_if_few(T).

/* compute the time alloted to each element of the d-r slot */
compute_time(D_exs,T) :-
        time(T1),
        length(D_exs,N),
        T is (T1 / 3 * 2)/N.

