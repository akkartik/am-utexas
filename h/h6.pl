/********************************************************
 * h6:  Any entity X is interesting if it is referred   *
 *      to in several interesting conjectures.          *
 ********************************************************/

/* The 'interesting' slot contains a list of three elements.
 * h6 would be the FIRST element,  The second element would
 * be 400, and the third element would be "A concept X is
 * interesting if X.Conjecs contains some interesting entries"
 */
h6(X) :-
        collect([conjectures,interest],[X],Conjecs_lst),
        h6_int_conjecs(Conjecs_lst,[],Int_conjs),
        non_null_list(Int_conjs).
/* akkartik: replaced periods with commas to respect indentation. */

/* The following takes a list of conjectures and adds those
   that are interesting to the list Oj, yielding the list,
   Jecs.
*/
h6_int_conjecs([H|T],Oj,Jecs) :-
        h6_interesting(H,J),
        h6_int_conjecs(T,[],Oj),
        append(J,Oj,Jecs).



/* The 'interesting' slot is a list of three elements.
 * The first element is the 'interesting' test predicate,
 * the second element is the interestingness value,
 * and the third element is the reason.
 * The following checks to see if the second element meets
 * the criterion of being interesting.  If so, it returns that
 * value in its second argument.  Otherwise it returns [].
 */
h6_interesting([F,S|R],[S]) :- interesting(S).
h6_interesting([F,S|R],[S]) :- veryinteresting(S).
h6_interesting(_,[]).

/* For now, we use the following values for 'interesting' */
veryinteresting(X) :- X >= 500.
interesting(X) :- X >= 350,
                  X < 500.
somewhatinteresting(X) :- X >= 200,
                          X =< 350.
boring(X) :- X < 200.

