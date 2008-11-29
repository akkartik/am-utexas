/*********************************************************
 * h46 anyconcept.examples.suggest
 *      If there are no known examples for the int con X,
 *      then look for examples of X
 */

h46(C) :-
        get(C,[worth],[W]),
        W > 500,
        exs(C,[]),
        addtoagenda(fillin,C,[examples],W,'no exs of an interesting con').

