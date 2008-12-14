:- public(h407/1).

/* if a concept is worthwhile then compose it with itself; this is short
of like Lenat's repetition heuristic. However, the only way that that
heuristic will work is if the domain = range! Thus one could be equal to
repetitive application of a concept and it might not. -marcos */

h407(F):- 
	get(F,[worth],[Worth]),
        Worth > 200,
        assertz(flag),
        getarity(F,N1), N is N1 -1,
        loop_composit2(F,F,N,[],Glist,[],FoGdr,1),
        makename(F,'_o_',Temp),
        loopmakename(Temp,Glist,SeedName),
        loop_make_composit(F,SeedName,Glist,FoGdr,Newname,Algorogo,N,0),!,
        assertz(flag),
        create_composite_concept2(F,Glist,Newname,Algorogo,FoGdr).        

loop_composit2(F,G,N,X,X,Y,Y,_):-
        N = 0.

loop_composit2(F,G,N,Glist,New_Glist,Old_FoGdr,New_FoGdr,It) :-
        get(F,[dom_range],Fdr),
        get(F,[dom_range],Gdr),
        get_composite_dr(Fdr,Gdr,FoGdr,It),
        N1 is N - 1,
        It2 is It + 1,
        loop_composit2(F,F,N1,[G|Glist],New_Glist,[FoGdr|Old_FoGdr],
		      New_FoGdr,It2).
