module(am, [am/0]).

compile([amutilities,utilities]).
consult([descriptions,concepts,definitions]).
load_am_files:-descr(H,_,_), strcat('h/',H,Hfile),consult(Hfile),fail.
load_am_files.

am :-
   init_am, 
   repeat,
     am_loop,
   fail.

init_am :- 
        abolish(agenda,1),
        abolish(time,1),
        abolish(history,1),
        abolish(do_threshold,1),
        abolish(seed,1),
        abolish(auto,1),
        abolish(cycle,1),
        assert(cycle(1)),
% unless am is broken you shouldn't need to reload the concepts every time
%        abolish(frame,3),
%        [concepts], 
        assert(auto(no)),
        assert(seed(13)),
        assert(time(0)),
        assert(history([])),
        assert(do_threshold(500)),
        assert(agenda([[fillin,set, [examples],310,[['some reason',10]]],
                       [suggest,set,[examples],300,[['why not',10]]],
                       [fillin,set, [genl],    300,[['whynot',10]]],
                       [fillin,set, [spec],    300,[['reason',10]]],
                       [check,set,  [examples],150,[[r1,10],[r2,50]]]])).


am_loop:-
   retract(cycle(Cycle)),NextCycle is Cycle + 1, assert(cycle(NextCycle)),
   amformat('~n---- Cycle ~a:  ', [Cycle]),
   user_selects_task(Task),
   Task = [Op,Con,Slot,Worth,_],
   compute_time(Worth),
   collect_heuristics(Con,Slot,Op,H),
   amformat('~a ~a of ~a  ----------------~n',[Op,Slot,Con]),
   execute_heuristics(Con,H),
   !.

user_selects_task(Task) :- 
%       present_choices,
%       aminput(Ans),
%       process_input(Ans,Task),
        process_input('',Task),
        !.

present_choices :-
        cdisplay(3),
        present_choice1,nl.

present_choice1 :- best_worth(W),do_threshold(Thresh),
    W < Thresh,
    amformat('Worth of best task is ~a, do-threshold is ~a,~n',
        [W,Thresh]),
    amformat('Best task is poor. # = task, s = suggest~n',[]),
    amformat('n = new threshold,i = input task,  q = quit,~n',[]),
    amformat('a = agenda display, p = concept printing, b = break. ~n', []).
        
present_choice1 :-
        printstring("hit return to select top task,"),
        printstring(" # = task #,q = quit, x = extensions "),nl,
    amformat('n = new threshold,i = input task,  q = quit,~n',[]),
    amformat('a = agenda display, p = concept printing, b = break. ~n', []).



process_input(q,[]).
process_input(n,Task) :- toptask(Task), set_new_threshold.
process_input(s,Task) :- suggest_task(T),
        apply(addtoagenda,T),toptask(Task).
process_input(x,Task) :- print('not yet implemented'),nl,toptask(Task).
process_input(i,Task) :- user_task,toptask(Task).
process_input(a,Task):- write('How many tasks do you want to display?'),
        aminput(D), cdisplay(D), user_selects_task(Task).
process_input(p,Task):-  
        write('Do you want to print a single concept or all of them?'),nl,
        write('name to print one, a for all, or <cr> for cancel'), aminput(A),
        processinput1(A),user_selects_task(Task).
process_input(b,Task):- break,user_selects_task(Task).
process_input(N,Task) :- integer(N),select_task(N,Task).
process_input(_,Task) :- toptask(Task),  !.
process_input(_,_):-
   nl, write('Agenda is empty.  AM is exiting.  Good-bye!'), nl, 
   abort.
processinput1([]):- !.
processinput1(a):- ppall.
processinput1(N):- ppframe(N).

set_new_threshold :- do_threshold(X),
        print('Old threshold is '),print(X),nl,
        print('New threshold is '),ttyflush,
        aminput(Y),set_new_threshold1(Y).
set_new_threshold1(Y) :- integer(Y),retract(do_threshold(_)),
        asserta(do_threshold(Y)).
set_new_threshold1(_) :- print('Must be an integer'),nl,
        set_new_threshold.
        
suggest_task(T) :-  best_worth(W),W1 is W + 500,
        agenda([[_,Con|_]|_]),
        T = [suggest,Con,_,W1,'there are no worthwhile tasks'].

/* allot time in 10ths of seconds = to Worth * 1.5. I.e.
 * a good task (worth 800) gets 12 seconds.
 */
compute_time(Worth) :- T is Worth * 3 / 2,
        retract(time(_)),
        assert(time(T)).
        
collect_heuristics(Con,[examples,typ],Op,H) :- 
        collect_heuristics(Con,[examples],Op,H).

collect_heuristics(Con,Slot,Op,H) :-
        genls_sf(Con,L),
        append(Slot,[Op],Slot_name),
        collect(Slot_name,L,H1),
        collect([Op],L,H2),
        append(H1,H2,H3),
        removedups(H3,H),!.

execute_heuristics(_,_) :- time(T),
        T < 0.
execute_heuristics(Con,[H|R]) :- 
        ((descr(H,Msg,_),
           format('~a:~t~8|~a~n',[H,Msg]),ttyflush) ;
         (format('~a:~t~8|~n',[H]), ttyflush)),
        clock(Start,_),
        apply_heuristic(H,[Con]),
        clock(Start,Elapsed_time),
        retract(time(T)),
        T1 is T - Elapsed_time,
        assert(time(T1)),!,
        execute_heuristics(Con,R).
execute_heuristics(_,[]).

% apply heuristic, if it fails then just succeed trivially.
% Could do some statistical collection here.

% This version stubbed out since success/failure info is not
% always an indication of the effect of a heuristic.  -Todd
%apply_heuristic(H,A) :-   apply(H,A), !,  amformat('succeeded ~n',[]).
%apply_heuristic(H,_) :-   amformat('failed ~n',[]).

apply_heuristic(H,A):- apply(H,A), !.
apply_heuristic(_,_).

% This really goes in amutilities.pl but it must be interpreted so it's here
print_put_trace(C,S,V):-
   ancestors([G|_]), % find out who's calling put/3,
   G=..[H|_],
   write(['   Adding',V,to,the,S,slot,of,C]), nl, % show change,
   !, fail.
