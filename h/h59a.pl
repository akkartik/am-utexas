/**********************************************************
 * h59a anyconcept.examples.check
 *
 * Insure that each example of C satisfies the defn of C.
 * If it does not, then see if it satisfies the defn of
 * a genl of C (only look at one step).
 */

h59a(C) :-
        examples(C,Exs),
        get(C,[genl],G),
        get(C,[examples,dif],[Num,Time]),
        get(C,[defn,name],[Defn]),
        getarity(C,Arity),
        time_per_example(Num,Time,Tper),
        h59a_check_exs(C,G,Defn,Arity,Exs,Tper).

time_per_example(Num,Time,Tper):-
        \+Num=0,
        Tper is Time / Num,
        Tper > 0.
time_per_example(Num,Time,0).

h59a_check_exs(_,_,_,_,[],_).
h59a_check_exs(C,G,Defn,Arity,[Ex|Exs],Tper) :-
        unifyinst(Arity,Inst,Defn,Ex),
        Inst,
        h59a_check_exs(C,G,Defn,Arity,Exs,Tper).
h59a_check_exs(C,G,Defn,Arity,[Ex|Exs],Tper) :-
        move_ex_up(G,Ex,Tper),
        remove_example(C,Ex,Tper),
        h59a_check_exs(C,G,Defn,Arity,Exs,Tper).

remove_example(C,Ex,Tper) :-
        fremove(C,[examples,typ],Ex),
        get(C,[examples,dif],[N,T]),
        N1 is N-1,T1 is T-Tper,
        update(C,[examples,dif],[N1,T1]).

/* stub this out for now.  should move ex to one of the gens if
   it satisifies gen.defn.
*/
move_ex_up(G,Ex,Tper).

