





















/* H29 - Any-concept.Examples.Fillin : 
   To fill in examples of X, where X is a kind of Y (for some more general
   concept Y), inspect the examples of Y; some of them may be examples of X
   as well.                                                               */

h29(Concept) :-
        get(Concept,[genl],[]),
        addtoagenda(fillin,Concept,[genl],200,
        'Generalizations might be helpful for finding examples.').
h29(Concept) :-
        time(Time),
        Allowed is Time / 4, 
        clock(Start,_),
        get(Concept,[defn,name],[Definition]),
        getarity(Concept,Arity),
        exs(Concept,Old),
        genls_sf(Concept,[Concept|Superset]),
        h29_do_while_time_1(Start,Allowed,Definition,Arity,Superset,Old,Examples),
        clock(Start,Elapsed),
        h29_add_and_check_new_values(Concept,[examples,typ],Examples,Elapsed).

/* h29_do_while_time_1(Start,Allowed,Definition,Superset,Prior,Examples) -
   Examples returns those examples of the concepts in Superset which satisfy
   the predicate Definition and can be identified within Allowed time.  Prior
   keeps track of what items have already been tried.                      */

h29_do_while_time_1(_,_,_,_,[],_,[]).
h29_do_while_time_1(Start,Allowed,_,_,_,_,[]) :-
        clock(Start,Elapsed),
        Elapsed > Allowed.
h29_do_while_time_1(Start,Allowed,Definition,Arity,
                [Genl|Superset],Prior,Examples) :-
        getarity(Genl,Arity),
        find_examples(Genl,Genl_examples),
        setdiff(Genl_examples,Prior,New_items),
        append(New_items,Prior,Old),
        h29_do_while_time_2(Start,Allowed,Definition,Arity,New_items,Exs),
        h29_do_while_time_1(Start,Allowed,Definition,Arity,
                Superset,Old,More_exs),
        append(Exs,More_exs,Examples).
h29_do_while_time_1(Start,Allowed,Definition,Arity,
                [Genl|Superset],Prior,Examples) :-
        h29_do_while_time_1(Start,Allowed,Definition,Arity,
                Superset,Prior,Examples).

/* h29_do_while_time_2(Start,Allowed,Definition,Arity,Genl_examples,Examples) -
   Examples returns those examples in Genl_examples which satisfy the 
   predicate Definition and can be identified within Allowed time.    */

h29_do_while_time_2(_,_,_,_,[],[]).
h29_do_while_time_2(Start,Allowed,_,_,_,[]) :-
        clock(Start,Elapsed),
        Elapsed > Allowed.
h29_do_while_time_2(Start,Allowed,Definition,Arity,[Ex|Genl_exs],[Ex|Examples]):-
        makeinst(Definition,Arity,Ex,Call),
        Call,
        h29_do_while_time_2(Start,Allowed,Definition,Arity,Genl_exs,Examples).
h29_do_while_time_2(Start,Allowed,Definition,Arity,[Non_ex|Genl_exs],Examples):-
        h29_do_while_time_2(Start,Allowed,Definition,Arity,Genl_exs,Examples).


/* h29_add_and_check_new_values(Concept,Slot,Values,Time) -
   New examples of Concept, if any, are added to example slot Slot, updating 
   the [examples,diff] slot of Concept and proposing the task of checking the
   examples of Concept.                                                    */
   
h29_add_and_check_new_values(Concept,Slot,Values,Time) :-
        get(Concept,Slot,Old_values),
        setdiff(Values,Old_values,New_values),
        length(New_values,Number),
        put(Concept,[examples,dif],[Number,Time]),
        review_new_values(Concept,Slot,New_values).
h29_add_and_check_new_values(Concept,Slot,Values,Time).

/* find_examples(Concept,Examples) -
   Examples returns the examples, (both boundary and typical), of Concept.  If
   no examples are available, a task is proposed to discover some.          */

find_examples(Concept,[Example|Examples]) :- 
        exs(Concept,[Example|Examples]),!.

find_examples(Concept,[]) :-
        addtoagenda(fillin,Concept,[examples,typ],200,
        'Examples might be helpful for finding examples of a specialization').

/* review_new_values(Concept,Slot,New_values) -
   If New_values is not null, its members are added to Slot of Concept and a
   task is proposed to check the values of Slot, otherwise, (no new values
   have been discovered), the task of finding values for Slot of Concept is
   reproposed.                                                            */

review_new_values(Concept,Slot,[Value|Values]) :-
        putvals(Concept,Slot,[Value|Values]),
        addtoagenda(check,Concept,Slot,300,
          'New examples of the concept have recently been defined').
review_new_values(Concept,Slot,_) :-
        addtoagenda(fillin,Concept,Slot,100,
          'Prior attempts to find examples failed, try again later').
