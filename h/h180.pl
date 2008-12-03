:-public(h180/1).

/* h180 finds examples of the composite operation f o g by using existing 
   examples of f and g. First the examples of f and g are gathered. Then a 
   matching pair is sought such that g's range(output) equals f's domain
   (input). This suggests that an example of f o g is [g's domain,f's range].
   Search proceeds by failure (of times_up) and backtracking.
   A task is added to the agenda to check the new examples(note that although
   addtoagenda may be called many times, only one task is created because
   the reason remains the same). */
h180(Concept):-
        time(Allotment),
        clock(Start,_),
        h180_do_while_time(Concept,Allotment,Start).
h180(_).

h180_do_while_time(Concept,Allotment,Start):-
        get(Concept,[compose],[[F,G]]),
        exs(F,Exs_f),
        exs(G,Exs_g),
        member(Example,Exs_g),
        split_last(Example,Gdom,Grange),
        member([Grange,Frange],Exs_f),
        append(Gdom,[Frange],New_example),
        put(Concept,[examples,typ],New_example),
        addtoagenda(check,Concept,[examples,typ],150,
                    'new examples of this concept'),
        times_up(Allotment,Start).
