/********************************************************
 * h17: A concept X is interesting if X.Conjecs         *
 *      contains some interesting entries.              *
 ********************************************************/

h17(Con) :-
        isas(Con, Con_set),
        collect([conjectures,interest],Con_set,Conjecs_lst),
        h17_int_conjecs(Conjecs_lst,[],Int_conjs),
        non_null_list(Int_conjs).
/* akkartik: replaced periods with commas to respect indentation. */

/* The following takes a list of conjectures and adds those
   that are interesting to the list Oj, yielding the list,
   Jecs.
*/
h17_int_conjecs([H|T],Oj,Jecs) :-
        h17_interesting(H,J),
        h17_int_conjecs(T,[],Oj),
        append(J,Oj,Jecs).

/* The 'interesting' slot is a list of three elements.
 * The first element is the 'interesting' test predicate,
 * the second element is the interestingness value,
 * and the third element is the reason.
 * The following checks to see if the second element meets
 * the criterion of being interesting.  If so, it returns that
 * value in its second argument.  Otherwise it returns [].
 */
h17_interesting([F,S|R],[S]) :- interesting(S).
h17_interesting([F,S|R],[S]) :- veryinteresting(S).
h17_interesting(_,[]).

