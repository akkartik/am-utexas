
/* this is another attemp to have a heuristic that makes multiple attempts
at doing something repetitively. I am assuming that the number of
repetitions it not crucial. And of course it runs both forwards and
backwards!  It also runs with just one var as input. That is to say
the above applies to the defns and the algs that this heuristic
creates! -marcos */


h408(C):-
	get(C,[worth],[Worth]),
	Worth >= 100,
	get(C,[defn,name],[CON_NAME]),
        getarity(C,N),
	do_while_time(C,CON_NAME,Worth,N).

do_while_time(C,Defn_Name,Worth,N):-
        Times is Worth/10,
	makename(Defn_Name,'_X_',Temp),
	makename(Temp,Times,NewC_Dname),
	makeit(Defn_Name,N,Worth,Alg,NewC_Dname),
	get(C,[dom_range],Dom_range), Conceptname = NewC_Dname,
/*	check_with_user(),*/
	write(Alg),
        create_composite_concept(C,NewC_Dname,Alg,Dom_range).
	
create_composite_concept(F,Conceptname,Alg,Dom_range):-
        put(Conceptname,[name],Conceptname),
        put(Conceptname,[defn,name],Conceptname),
        put(Conceptname,[alg],Conceptname),
        assertz(frame(Conceptname,[dom_range],Dom_range)),
        put(Conceptname,[genl],F),
        get(F,[worth],[W1]),
        put(Conceptname,[worth],W1),
        addtoagenda(fillin,Conceptname,[examples],W1,
                    'no examples of this new concept').



makeit(D,N,Worth,Alg,Name):-  
	Count is Worth/10,
	makelist(N,List),
        split_last(List,Domain,Range),
	split_last(Domain,D1,D2),
	append(Domain,[D2],List2),
	append(Domain,[OutPut],List4),
	append(D1,[Range,OutPut],List3),
	Pred1 =.. [D|List],
	Pred2 =.. [Name|List2],
	Pred3 =.. [Name|List3],
	Pred4 =.. [Name|List4],
        makename(Name,'count',NewName),
	CP1 =.. [NewName|[Count1]],
	CP2 =.. [NewName|[Count2]],

        C0 = (Pred2:- CP1,Count1 > Count, retract(CP2)),
	C1 = (Pred4:- Pred1, (retract(CP1);
	                      Count1 = 0), Count2 is Count1 + 1, 
			      assertz(CP2),
	                      Pred3),
        assertz(C1),
	asserta(C0),nl, write(C0),nl, write(C1),nl,Alg=[C0,C1].




/*
callit(D,[],WholeExs,N,Output,Count,Worth,Start,LastR):-
	End = Start + 20,
        clock(End,End2),
        End2 < 0,
	callit(D,WholeExs,WholeExs,N,Output,Count,Worth,Start,LastR).

callit(Dname,[E1|Rest],WholeExs,N,Output,Count,Worth,Start,LastR):-
	End = Start + 10,
        clock(End,End2),
        End2 < 0,
	split_last(E1,Range,Dom),
        ((var(LastR),Range2 = Range);
         (split_last(Range,[R1],R2),
	  append(R1,LastR,Range2)))
	append(Range2,[Dom2],RDList),
        Pred =..[Dname|RDList],
	Count2 is Count + 1,
	callit(Dname,Rest,WholeExs,N,Output,Count2,Worth,Start,Dom2).

callit(Dname,_,_,N,Output,Count,Worth,Start,Output):-
	End = Start + 10,
        clock(End,End2),
        End2 >= 0.




         */
        

