:-public(h183/1).

/* h183 checks to see if f o g (or actually ANY concept) might be the same as
   another concept by comparing their examples. Neighboring concepts are
   tested in order of increasing distance from the concept in the concept tree
   (as long as time remains). When a presumed equivalence is detected, a 
   conjecture is asserted. Search progresses by failure (of times_up) and
   bactracking. */
h183(Concept):-
        time(Allotment),
        clock(Start,_),
        h183_do_while_time(Concept,Allotment,Start).
h183(_).

h183_do_while_time(Concept,Allotment,Start):-
        equivalent_concepts(Concept,Other_concept),
        assert_conjectures(Concept,Other_concept),
        times_up(Allotment,Start).

/* equivalent_concepts finds a neighbor to the concept being tested and
   compares their example lists. This is done by intersecting the lists and
   then comparing the length of the result to the lengths of the original
   lists to see if all lengths are the same. If the concepts have the same
   examples, they are conjectured to be equivalent. */
equivalent_concepts(Concept,Other_concept):-
        neighborconcept(Concept,Other_concept),
        exs(Concept,Exs1),
        exs(Other_concept,Exs2),
        intersection(Exs1,Exs2,Int),
        length(Exs1,L1),
        length(Exs2,L2),
        length(Int,L1),
        length(Int,L2).

assert_conjectures(Concept,Same_concept):-
        put(Concept,[conjecs],[equal,Concept,Same_concept]),
        put(Same_concept,[conjecs],[equal,Same_concept,Concept]),
        nl,nl,
        write('I have conjectured that '),
        write(Concept),write(' and '),write(Same_concept),nl,
        write(' are really the same.'),nl.

