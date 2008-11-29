/**********************************************************
 * h45 anyconcept.examples.suggest
 *      if very many examples of X are found in a short
 *      period of time, then specicialize X.
 */

h45(X) :-
        get(X,[examples,dif],[N,T]),
        N > 25,         % alot of examples
        (T / N) < 2,    % easy to find ?
        get(X,[worth],[W1]),
        W is W1 / 9 * 10,
        addtoagenda(fillin,X,[specs],W,'it was too easy to find Xs').

