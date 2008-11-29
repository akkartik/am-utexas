/*************************************************************
 *   h12 anything.suggest
 *      fillin any blank facet of any concept
 *      Takes forever.
 */

h12(_) :-
        time(T),
        T > 500,
        allconcepts(C),fillable_slots(S),
        member(X,C),member(Y,S),get(X,Y,[]),
        addtoagenda(fillin,X,Y,100,'no value currently defined'),
        fail.
h12(_).

