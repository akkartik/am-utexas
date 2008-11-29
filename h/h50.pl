/********************************************************
 * h50 anyconcept.examples.suggest
 *      After filling in examples of C, if some examples
 *      were found, check the examples of C

 *      NB: we should really check the history for 
 *      1.      if we recently got the exs
 *      2.      if we have already checked them
 */
h50(C) :- get(C,[examples,dif],[N,T]),
        N > 0,
        get(C,[worth],[W1]),
        W is W1 / 5,
        addtoagenda(check,C,[examples],W,'after finding some, check them').

