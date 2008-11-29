/* H40 - Any-concept.Examples.Fillin :
   Finds examples of Concept by checking "first cousins" of Concept, (ie: the
   immediate specializations of the immediate generalizations of Concept). */

h40(Concept) :-
        get(Concept,[genl],[]),
        addtoagenda(fillin,Concept,[genl],200,
        'Generalizations might be helpful in finding some examples.').
h40(Concept) :-
        time(Time),
        Allowed is Time/3,
        clock(Start,_),
        exs(Concept,Old),
        get(Concept,[defn,name],[Definition]),
        get(Concept,[genl],Generalizations),
        mysetof(Cousin_example,Generalization^Cousin^Cousin_exs^
                (member(Generalization,Generalizations),
                 h34a_find_values([spec],Generalization,Cousins),
                 member(Cousin,Cousins),
                 nonmember(Cousin,[Concept]),
                 find_examples(Cousin,Cousin_exs),
                 member(Cousin_example,Cousin_exs),
                 nonmember(Cousin_example,Old)),
                Cousin_examples),
        getarity(Concept,Arity),
        h29_do_while_time_2(Start,Allowed,Definition,Arity,Cousin_examples,Examples),
        clock(Start,Elapsed),
        h29_add_and_check_new_values(Concept,[examples,typ],Examples,Elapsed).
