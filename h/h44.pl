/***********************************************************
 *      h44 anyconcept.examples.suggest
 *      If very few examples of X are found, then add
 *      generalize X, since a less restrictive concept may
 *      be more interesting.
 */
h44(C) :-
        exs(C,Exs),
        length(Exs,Num),
        Num > 5,
        get(C,[worth],[W1]),
        W is W1 * 10 / 9,
        addtoagenda(fillin,C,[genl],W,
                'Xs are quite rare, less restrictive concept is more int').

