/********************************************************
 *                      h20                             *
 *      A concept is interesting if it is --            *
 *      accidentally -- precisely the boundary          *
 *      of some other, interesting concept.             *
 ********************************************************/

/* In this case the given concept might, itself, be
 * the boundary (as opposed to having its boundary
 * being equal to another guy's boundary).
 */

h20(Con) :- 
        collect(Con,[examples,bnd],Blist),
        allconcepts(Clist),
        interestingp(Clist, Int_c),
        mycollect([examples,bnd],Int_c,Many_blists),
        member(Con,Many_blists).


/* interestingp finds among the list of concepts, C,
 * those that are interesting
 */
%interestingp([C|Rlist],Intclist) :-
%       int(C,

/* I use mycollect to give me a list of lists,
 * instead of a single, big list.
 */
mycollect(_,[],[]).
mycollect(Slot,[H|T],L) :-
        get(H,Slot,L1),
        mycollect(Slot,T,L2),
        cons(L1,L2,L).

