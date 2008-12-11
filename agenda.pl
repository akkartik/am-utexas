:- public([
        toptask/1,best_worth/1,do_threshold/1,addtoagenda/5,
        select_task/2,current_task/1, display_tasks/1,cdisplay/1,
        current_worth/1,delete_task/1,user_task/0]).

% toptask(-Task_record) returns the highest priority task on the agenda.
% The task is removed from the agenda and added to the history when selected
toptask(Task):-
        retract(agenda(Agenda)),
        split(Agenda,[Task|Agenda2]),
        addtohistory(Task),
        assertz(agenda(Agenda2)).

/* best_worth(Worth of top task) gets the worth of the best task */
best_worth(0):-agenda([]).
best_worth(Worth) :- agenda([[_,_,_,Worth,_]|_]).

/* do_threshold returns the lowest acceptable value for an
 * executable task.
 */
do_threshold(500).

/* addtohistory adds a task to the 5 task history stack which is kept by
   the system. When a new task is selected for execution it is pushed
   onto the stack */
addtohistory(Task):-
        retract(history(History)),
        addh1(Task,History,H2),
        assertz(history(H2)).

/* this is a help function used by addtohistory. It adds a new task to the
   assertzed history clause, removing the oldest task if there are already 10 */
addh1(Task,[],[Task]).
addh1(Task,History,[Task|History]):-
        length(History,L),
        L<10.
addh1(Task,History,[Task|H2]):-
        removelast(History,H2).

/* addtoagenda adds a new task to the agenda if it is not already there.
   Its parameters are operation, concept, facet, worth, and reason:
               addtoagenda(+Op,+C,+F,+W,+R). */

/* if task is already there with same reason, do nothing */
addtoagenda(Op,C,F,W,R):-
        agenda(Agenda),
        member([Op,C,F,_,Rlist],Agenda),
        member([R,_],Rlist),!.
/* if task has been executed within the last 5 cycles and it's not worthy (<300)
, */
/*  do nothing. */
addtoagenda(Op,C,F,W,R):-
        W<300,
        history(History),
        member([Op,C,F,_,Rlist],History),
        member([R,_],Rlist),!.
/* if task is there with other reasons, add new reason and recompute worth */
addtoagenda(Op,C,F,W,R):-
        agenda(Agenda),
        member([Op,C,F,_,Rlist],Agenda),
% nl, cwrite('adding ',[Op,C,F,W,R]),
        newworth(Op,C,F,[[R,W]|Rlist],Worth),
        remove([Op,C,F,_,_],Agenda,Agenda2),
        addinorder([Op,C,F,Worth,[[R,W]|Rlist]],Agenda2,Agenda3),
        retract(agenda(_)),
        assertz(agenda(Agenda3)),!.
/* if task is not there, add task to agenda */
addtoagenda(Op,C,F,W,R):-
% cwrite('adding ',[Op,C,F,W,R]),
        agenda(Agenda),
        newworth(Op,C,F,[[R,W]],Worth),
        addinorder([Op,C,F,Worth,[[R,W]]],Agenda,Agenda2),
        retract(agenda(_)),
        assertz(agenda(Agenda2)),!.

/* newworth computes the worth of a concept using the formula:
      worth=(2*operator worth + 3*concept worth + 5*facet worth)
            * sum of reason worths / 1000.   */
newworth(Op,C,F,Rlist,Worth):-
        worth(Op,Oworth),
        worth(C,Cworth),
        worth(F,Fworth),
        getrworth(Rlist,Rworth),
        Ocf is (2*Oworth)+(3*Cworth)+(5*Fworth),
        Worth is (Rworth*Ocf)/1000.

/* getrworth is a help function for newworth that sums the worths of
   the reasons for a task  */
getrworth([],0).
getrworth([[R,W]|Tail],Rworth):-
        getrworth(Tail,Rw2),
        Rworth is W+Rw2.

/* addinorder adds a new task to the agenda list in priority order. If
   an existing task has the same priority, the new task goes ahead of
   it to give recent tasks a slight priority edge */
addinorder(Task,[],[Task]).
addinorder(Task,Agenda,[Task|Agenda]):-
        split(Task,[_,_,_,W,_]),
        split(Agenda,[[_,_,_,W2,_]|Tail]),
        W>=W2.
addinorder(Task,[H|T],[H|Agenda2]):-
        addinorder(Task,T,Agenda2).

/* select_task allows selecting a task other than the highest priority
   task from the agenda. If the task number exceeds the actual number of
   tasks in the agenda, the top task is returned. The task is removed from
   the agenda and added to the history.
             select_task(+Task_number,-Task_record).      */
select_task(N,Task):-
        agenda(Agenda),
        length(Agenda,L),
        N>L,
        toptask(Task).
select_task(1,Task):-
        toptask(Task).
select_task(N,Task):-
        retract(agenda(Agenda)),
        nth(Agenda,N,Task),
        addtohistory(Task),
        remove(Task,Agenda,Agenda2),
        assertz(agenda(Agenda2)).

/* current_task(-Task_record) returns the record of the currently executing
   task from the history stack. */
current_task(Task):-
        history([Task|_]).
current_task([]).

/* display_tasks(+Number_to_display) prints the top N tasks from the agenda
   if N exist */
display_tasks(N):-
        agenda(Agenda),
        disp(N,1,Agenda).

/* disp is a help function for display_tasks */
disp(0,_,_):-
        nl,nl.
disp(_,_,[]):-
        nl,nl.
disp(N,Count,[H|T]):-
        wrtask(H,Count),
        N1 is N-1,
        C2 is Count+1,
        disp(N1,C2,T).

/* wrtask is a help function that displays the information for a single
   task */
wrtask([Op,C,F,W,Rlist],Count):-
        nl,
        write('Task # '),write(Count),nl,
        write('  operator: '),write(Op),nl,
        write('  concept:  '),write(C),nl,
        write('  facet:    '),write(F),nl,
        write('  worth:    '),write(W),nl,
        write('  reasons:  '),nl,
        writerlist(Rlist).

/* writerlist is a help function for wrtask which writes out the reason
   list  */
writerlist([]).
writerlist([[R,W]|T]):-
        write('            '),write(R),write('   '),write(W),nl,
        writerlist(T).

/* cdisplay(+Number_of_Tasks) concisely displays the desired number of tasks
   from the top of the agenda. The tasks are displayed one per line; each line
   contains the task number, operation, concept, facet, and worth. Reasons are
   not displayed. cwrite is used to print a heading.    */
cdisplay(N):-
        agenda(Agenda),
        length(Agenda,Num),
        amformat('~a Tasks on agenda~n',[Num]),
        cwrite(' ',['OPERATION','CONCEPT','FACET','WORTH',unused]),
        cdisp(N,1,Agenda).

/* cdisp is a help function for cdisplay  */
cdisp(0,_,_):-nl,nl.
cdisp(_,_,[]):-nl,nl.
cdisp(N,Count,[H|T]):-
        cwrite(Count,H),
        C1 is Count+1,
        N1 is N-1,
        cdisp(N1,C1,T).

/* cwrite writes a one line concise task entry  */
cwrite(N,[Op,C,F,W,_]):-
        write(N),
        spaces(N,5),
        write(Op),
        spaces(Op,15),
        write(C),
        spaces(C,20),
        write(F),
        spaces(F,30),
        write(W),nl.

/* spaces is a function to fill the difference in length between the length of
   a given atom and the print field into which it is being written. It is 
   used to align atoms for columnar output  */


spaces(X,N):-
        explode(X,List),
        length(List,L),
        T is N-L,
        tab(T).

/* current_worth(-W) returns the worth of the currently executing task from
   the top of the history. */
current_worth(0):-
        history([]).
current_worth(W):-
        history([[_,_,_,W,_]|_]).

/* delete_task(+task_number) deletes the task numbered N from the agenda. Tasks
   are numbered starting with 1 as the highest priority task. */
delete_task(N):-
        agenda(Agenda),
        length(Agenda,L),
        (N<1;N>L).
delete_task(N):-
        retract(agenda(Agenda)),
        nth(Agenda,N,Task),
        remove(Task,Agenda,Agenda2),
        assertz(agenda(Agenda2)).

/* user_task is invoked to allow the user to define his/her own task. The worth
   is fixed at 500 and the reason is fixed. */
user_task:-
        write('Input information about the new task.'),nl,
        write('End with a period since the Prolog read is used here.'),nl,
        write('Operation: '),ttyflush,
        read(Op),nl,
        write('Concept: '),ttyflush,
        read(C),nl,
        write('Facet: '),ttyflush,
        read(F),nl,
        addtoagenda(Op,C,[F],500,'User Requested Task').
