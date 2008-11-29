/*** h300 and h301 respectively generalize and specialize a concepts definition
**** by finding the most interesting generalization or specialization of its 
**** most interesting component predicate.  For example, h300(set_member) would
**** work as follows
****    Given the definition of set_member as
****          set_member_defn([H|_],Element):-
****             basecase,
****             set_equal_defn(Element,H).
****          set_member_defn([_|T],Element):-
****             set_member_defn(T,Element).
****    h300 might pick the component predicate set_member_defn as the most
****     interesting, and generalize it to member_defn to generate the new
****     concept:
****          genl_of_set_member_defn([H|_],Element):-
****             basecase,
****             set_equal_defn(Element,H).
****          genl_of_set_member_defn([_|T],Element):-
****             genl_of_set_member_defn(T,Element).
**** h301 works similarly in the specialization direction.
***/


h300(Con):-
   time(T), 
   T1 is T/2,
   clock(Start,_), !,
   mutate_defn(Con,genl,T1,Start).
h300(_).

mutate_defn(Con,Direction,Alotment,Start):-     % Direction = up or down
   get(Con,[dom_range],Check), \+ Check = [],   % Ensure dom_range slot exists
   most_interesting_predicate(Con,Con_defn,MI_pred_defn,Con_clauses),
   strip__defn(MI_pred_defn,MI_pred),
   convert_direction(Direction,Root,Relation,Rel), !,
   most_interesting_mutation(MI_pred,Rel,Con,MI_mutation_defn),
   % Give it a new name
   newdefname(Root,Con,NewCon_defn),
   replacePredName(NewCon_defn,Con_clauses,Con_clauses1),
   % Mutate the definition...
   totalreplace(MI_pred_defn,MI_mutation_defn,Con_clauses1,Con_clauses2),
   totalreplace(Con,NewCon_defn,Con_clauses2,NewCon_clauses),
   % ---A new concept is born!  Make sure it's unique...
   not_already_defined(NewCon_defn,NewCon_clauses),
   check_with_user(Con,Relation,NewCon_defn,NewCon_clauses,Final_con,Final_con_clauses),
   % It's unique and the user likes it -> save it.
   assertset(Final_con_clauses),
   updateconcepts(Con,Final_con,Direction),
   put_in_hierarchy(Con,Final_con), 
   times_up(Alotment,Start),  !.
mutate_defn(Con,Direction,_,_):-
   get(Con,[dom_range],[]),
   addtoagenda(fillin,Con,[dom_range],500,'No examples of this slot exist'),
   if(Direction = genl,
      Reason = 'Generalize C by generalizing a predicate in its defn',
      Reason = 'Specialize C by specializing a predicate in its defn'),
   addtoagenda(fillin,any_concept,[Direction,fillin],400,Reason),  !.


convert_direction(genl,genl_of_,generalization,up).
convert_direction(spec,spec_of_,specialization,down).

% Expects a list of lists (ie. output from collectclauses), and gives the
%  predicate they define a new name.
replacePredName(_,[],[]).
replacePredName(NewFunctor,[ [OldHead|Body] | OldRest], [ [NewHead|Body] | NewRest] ):-
   OldHead=..[_|OldArg],
   NewHead=..[NewFunctor|OldArg],
   replacePredName(NewFunctor, OldRest, NewRest).

   
strip__defn(Old,New):-
   name(Old,String_defn),
   append(String,[95,100,101,102,110|_],String_defn),  !,
   name(New,String).
strip__defn(Old,Old).

most_interesting_predicate(Con,Con_defn,MI_pred,Con_clauses):-
   get(Con,[Defn,Name],[Con_defn]),
   getarity(Con,Arity),
   collectclauses(Con_defn,Arity,Con_clauses),
   setof(Pred2s, X ^ (member(X,Con_clauses), conditionof(Pred2s, X) ), List_O_Preds),  !,
   most_interesting_pred1(List_O_Preds,MI_pred).


% given a list of predicates, returns most interesting predicate functor.
most_interesting_pred1([Pred], Pred_functor):- Pred=..[Pred_functor|_].
most_interesting_pred1([Pred|Preds], MI_Pred):-
   most_interesting_pred1(Preds, Pred1),
   if(frame(Real_Pred1, [defn,name], [Pred1]),     % if the concept exists...
         get(Real_Pred1, [worth], [P1_worth]),     %    get its worth
         P1_worth = 0),                            %   else use zer0.
   Pred=..[Pred_functor|_],
   if(frame(Real_Pred, [defn,name], [Pred_functor]), % if the concept exists...
      get(Real_Pred, [worth], [P_worth]),            %    get its worth
      P_worth = 0),                                  %   else use zerO.
   if(P1_worth>=P_worth,
      MI_Pred = Pred1,
      MI_Pred = Pred).

most_interesting_mutation(MI_pred,Rel,Con,MI_mutation_defn):-
   ripple_sf(Rel,MI_pred,X_cons1),      % List of gens/specs of MI_functor.
   remove(Con,X_cons1,X_cons),          % Con is not a mutation of itself.
   screen_mutants(Con,X_cons,Valid_X_cons),  !,
   most_interesting_concept(Valid_X_cons,MI_mutation),
   get(MI_mutation,[defn,name],[MI_mutation_defn]).

most_interesting_clause([C],C).
most_interesting_clause([C|Cs],C):-
   C =..[C_func|_],
   get(C_func,[worth],[C_worth]),
   most_interesting_clause(Cs,MI_rest),
   MI_rest =..[MI_rest_func|_],
   get(MI_rest_func,[worth],[C_worth]),  !,
   C_worth >= MI_rest_worth.
most_interesting_clause([_|Cs],MI):-
   most_interesting_clause(Cs,MI).
% This part just enables backtracking to resucceed with the 2nd most interesting
% predicate, etc.
most_interesting_concept(C,MIC):- most_interesting_concept2(C,MIC).
most_interesting_concept(C,MIC):-
   most_interesting_concept2(C,OldMIC),
   remove(OldMIC,C,NewC),
   most_interesting_concept(NewC,MIC).

% Finds the worth of the first concept and returns it if it's better than all the rest.
most_interesting_concept2([C],C).
most_interesting_concept2([C|Cs],MI_C):-
   get(C,[worth],[C_worth]),
   most_interesting_concept2(Cs,MI_rest),
   get(MI_rest,[worth],[MI_rest_worth]),
   if( C_worth >= MI_rest_worth,
      MI_C = C,
      MI_C = MI_rest),  !.
most_interesting_concept2([_|Cs], MI_C):-
   most_interesting_concept2(Cs, MI_C).
   
% check all that domain/range compatibility stuff...
screen_mutants(Cons,X_cons,Valid_cons):-
   getarity(Cons,Arity),
   dom_ran(Cons,Domain,Range),
   screen_them_mutants(Arity,Domain,Range,X_cons,Valid_cons).

screen_them_mutants(_,_,_,[],[]).
screen_them_mutants(A,D,R,[H|T],[H|Rest]):-
   getarity(H,A),
   dom_ran(H,D2,R2),
   subset(D,D2),
   subset(R2,R),
   screen_them_mutants(A,D,R,T,Rest).
screen_them_mutants(A,D,R,[_|T],Rest):-
   screen_them_mutants(A,D,R,T,Rest).

dom_ran(Cons,Domains,Ranges):-
   get(Cons,[dom_range],DR),
   split_dom_range(DR,Domains,Ranges).

split_dom_range([],[],[]).
split_dom_range([DR|DRs],[D|DT],[R|RT]):-
   split_last(DR,D,R),
   split_dom_range(DRs,DT,RT).

subset([],_).
subset([X|Xs],Y):-
   member(X,Y),
   subset(Xs,Y).

