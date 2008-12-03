:- public([
         split/2,null/1,not_null_list/1,consp/1,cons/3,split_last/3,
         firstn/3,reverse/2,removeall/3,remove/3,
         remove_or_die/3,
         removelast/2,nth/3,wrlist/1,myinput/1,list/1,flatten/2,
         lastof/2,append/3,concat/3,replace/4,assoclist/3,union/3,
         setdiff/3,intersection/3,
         makeset/2,merge/3,setmember/2,

         member/2,seteq/2,delete/3,absval/2,mysetof/3,mybagof/3,
         /*ucall/1,*/clock/2,gensym/2,myprint/2,pp/1,printstring/1,
         printstrings/1,makename/3,

         collectclauses/3, makelist/2, if/2, if/3,

         explode/2,random/2,removedups/2,removetop/3,setdif/3,
         apply/2,format/3,format/2,prompt_and_read/3,randomelement/2,
         remove_random/2,randombreak/3,remove_nth/3]).

split(X,X).

null([]).

not_null_list([_|_]).
consp([_|_]).
cons(A,B,[A|B]).

/* Split_last(+List,-Allbutlast,-Last) */
split_last([A,B],[A],B).
split_last([H|T],[H|T1],Last) :- split_last(T,T1,Last).

firstn([],N,[]).
firstn([H|Remlist],N,[H|Remfirst]):- 
  N>0, N1 is N-1,firstn(Remlist,N1,Remfirst).
firstn(List,N,[]):- \+N>0.



%? reverse(L,L1):- reverse_concat(L,[],L1).
%? reverse_concat([X|L1],L2,L3):- reverse_concat(L1,[X|L2],L3).
%? reverse_concat([],L,L).


removeall(Set1,[],Set1).
removeall(Set1,[Del|Rest],Set2):- remove(Set1,Del,SetX), 
  removeall(SetX,Rest,Set2).

remove(_,[],[]).
remove(Member,[Member|Rest],Rest).
remove(Member,[H|Rest],[H|Newrest]):- \+H=Member, remove(Member,Rest,Newrest).

remove_or_die(Member,List,Newlist) :- member(Member,List),
        remove(Member,List,Newlist).

removelast([X],[]).
removelast([X|Y],[X|Z]) :- removelast(Y,Z).

%? nth(L,P,V):- nth2(L,P,V,1).
%? nth2([H|T],N,H,N).
%? nth2([H|T],P,V,N):- \+P=N, N1 is N+1, nth2(T,P,V,N1).

%length([],0).
%length([X|Y],N) :- length(Y,N1), N is N1 + 1.

wrlist([]).
wrlist([H|Rest]):- write('   '),write(H),nl,wrlist(Rest).

%? list([]).
%? list([_|_]).

flatten(Atom,Atom):- \+list(Atom).
flatten(L,F):- list(L), flatten2(L,F),!.
flatten2([],[]).
flatten2([X],[X]):- \+list(X).
flatten2([X|Y],Z):- flatten2(X,X1),flatten2(Y,Y1),append(X1,Y1,Z).
flatten2(X,[X]):- \+list(X).

lastof(L,[L]).
lastof(L,[A|B]):- lastof(L,B).

/* myinput allows character input ending with CR, no periods needed */
myinput(I):- myread(T),reverse(T,R), name(I,R).
myread(I):- myread2([],I).
myread2(Prev, More):- ttyget0(C), \+C=10, % 10 in quintus prolog
   myread2([C|Prev],More),!.
myread2(A,A):- !.
 
%? append([],L,L).
%? append([A|B], L2, [A|L3]):- append(B,L2,L3).

concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).
concat([],L,L).

strcat(H,T,HT):-name(H,HL), name(T,TL), append(HL,TL,HTL), name(HT,HTL).

replace(Old,New,[Old|Rest],[New|Rest]).
replace(Old,New,[Car|Oldlist],[Car|Newlist]):- 
               replace(Old,New,Oldlist,Newlist).

/* assoclist(Oldlist,list of pairs,Newlist)  */

assoclist(List,[],List).
assoclist(List,[H|Tail],L2):-assocterm(List,H,L1),
                             assoclist(L1,Tail,L2).

/*
assocterm(Lhs=>Rhs, H, Nlhs=>Rhs):- assocterm(Lhs, H, Nlhs).
*/
assocterm([],H,[]):- !.
assocterm([F|B],H,[F1|B1]):-ass1(F,H,F1),assocterm(B,H,B1).
assocterm(T,H,T2):- ass1(T,H,T2),!.

ass1(H,[H,T],T):- !. /* no choice here  */
/*
ass1(X,Pr,T):-  X=..[eq,Eqno|T1], assocterm(T1,Pr,T2), T=..[eq,Eqno|T2].
ass1(X,Pr,T):- X=..[*|Ts], assocterm(Ts,Pr,T2), T=..[*|T2].
*/
ass1(X,Pr,T):- X=..[X1|T1], assocterm(T1,Pr,T2), T=..[X1|T2].
ass1(X,Pr,X).

union(L,[],L).
union([],L,L).
union([H|T],L,L1):-member(H,L),union(T,L,L1).
union([H|T],L,[H|L1]):-union(T,L,L1).


/* setdiff(Set1,Set2,Diff) : Diff returns the elements in Set1 not in Set2 */

setdiff([],_,[]).
setdiff([H|T],Set2,[H|Diff]) :- 
        \+ member(H,Set2),
        setdiff(T,Set2,Diff).
setdiff([H|T],Set2,Diff) :-
        setdiff(T,Set2,Diff).


   intersection([],L,[]).
   intersection([H|A1],A2,[H|L]) :-
              member(H,A2),
              intersection(A1,A2,L).
   intersection([H|A1],A2,L) :- 
              intersection(A1,A2,L).

makeset(Bag,Set) :- mysetof(X,member(X,Bag),Set).

merge([],L,L).
merge([H|T],L,L1):-setmember(H,L),merge(T,L,L1).
merge([H|T],L,L1):-merge(T,[H|L],L1).

setmember(H,[H1|L]):-seteq(H,H1).
setmember(H,[_|L]):-setmember(H,L).

%? member(X,[X|T]).
%? member(X,[_|T]):-member(X,T).


seteq([],[]).
seteq([A|B],C):-delete(A,C,C1),seteq(B,C1).

%? delete(A,[A|B],B).
%? delete(A,[B|C],[B|C1]):-delete(A,C,C1).

absval(N,N):- integer(N), \+N<0.
absval(N,AbsN):- integer(N), N<0, AbsN is -1*N.


call2(X):- \+list(X), call(X).          % Choice can be a single goal...
call2([]).                              %    ... or a list of goals.
call2([H|T]):- call(H), call2(T).

% This IF does not try to resatisfy the condition if it fails (as the QP built
% in '->' does).  It behaves exactly as if-then, if-then-else structures of
% other languages do.
% NOTE: The if, then, and else slots may be single predicates, or lists of
%   predicates. ie: if( [foo(x),bar(y)] , write(foobar) , [write(no),nl] ).
% NOTE: IF will ALWAYS succeed, hence it's invisible to the goal satisfaction
%   process.
% WARNING: The clauses in lists are evaluated regardless of their resulting
%   value, so in the above example, suppose the test fails, the 'nl' would be
%   evaluated even if the 'write' failed for some reason.
if(If,Then):- if(If,Then,true).
if(If,Then,_):-   call2(If), call2(Then), !.
if(If,_,Else):-   call2(Else).



 /* mysetof & mybagof return [] if there are no values that satisfy
 * P(X), rather than failing as do setof & bagof.
 */
mysetof(A,B,C) :- setof(A,B,C).
mysetof(_,_,[]).

mybagof(A,B,C) :- bagof(A,B,C).
mybagof(_,_,[]).

/* t(X) gives time since last call to statistics.  It is not
 * generally as useful as clock(_,_) since intermediate calls
 * to t(_) will reset the time.
 */

t(X) :- statistics(runtime,[_,X]).


/***
**** turn a list into a function call
***/
%? :-op(100,fx,ucall).
%? ucall(X) :- Z =.. X ,Z.

/* Create a new atom starting with a root provided and 
 * finishing with a unique number.
 */
gensym(Root,Atom) :-
        get_num(Root,Num),
        name(Root,Name1),
        name(Num,Name2),
        append(Name1,Name2,Name),
        name(Atom,Name).

get_num(Root,Num) :-
        retract(current_num(Root,Num1)), !,
        Num is Num1 + 1,
        asserta(current_num(Root,Num)).
get_num(Root,1) :- asserta(current_num(Root,1)).


/* convert from an integer to a list of chars */

integer_name(Int,List) :- integer_name(Int,[],List).

integer_name(I,Sofar,[C|Sofar]) :-  I < 10, !, C is I + 48.
integer_name(I,Sofar,List) :- Top is I / 10,
        Bot is I mod 10,
        C is Bot + 48,
        integer_name(Top,[C|Sofar],List).

/* print a list of atoms with spaces between them, return length
 * of all printed chars (len of atoms + spaces)
 */
myprint([],0).
myprint([H|T],Len) :-
        name(H,L),length(L,Len1),
        write(H),write(' '),
        myprint(T,Len2), Len is Len1 + Len2 +1.

/*      'list' pretty printer  with brackets    */
/*              ---  Martin Purvis              */

pp(X) :-        write('['),
                pp_aux(X,1),
                write(']').

pp_aux([],_).
pp_aux([[HH|HT]|T], I) :- J is I + 1,
                          write('['),
                          pp_aux([HH|HT],J),
                          write(']'),
                          pp_aux2(T,I).

pp_aux([H|T],I) :-  pp_aux(H,I), 
                    pp_aux2(T,I).
pp_aux(X,I) :-  write(X).

pp_aux2(X,I) :- null(X).
pp_aux2(X,I) :-     nl,
                    tab(I),
                    pp_aux(X,I).





/* print a string */
printstring([]).
printstring([H|T]) :- put(H), printstring(T).

/* print a list of strings */
printstrings([]).
printstrings([H|T]) :- printstring(H),printstrings(T).

makename(X,Y,N) :- name(X,X1),name(Y,Y1),append(X1,Y1,N1),
        name(N,N1).



%? /*** collectclauses forms a list of all clauses with a given mainfunctor.
%? **** The only tricky part is forming a template which will match the
%? **** head of each of the clauses (this to satisfy the 'clause' predicate).
%? ***/
%? 
%? collectclauses(Mainfunctor,N,Clauses):- functemplate(Mainfunctor,N,Func),
%?   bagof([Func,Body], clause(Func,Body), Clauses).
%? functemplate(Mainfunctor,N,Func):- makelist(N,L), Func=..[Mainfunctor|L].
%? 
%? /*** makelist(+N,-L) forms a list L of length N of uninstantiated variables. */
%? 
%? makelist(0,[]).
%? makelist(N,[_|L]):- N>0, N1 is N-1, makelist(N1,L).




explode(Var,X) :- var(Var),gensym(A,X).
explode(Atom,L) :- atomic(Atom),name(Atom,L).
explode(L,E) :- list(L),
        numbervars(L,1,_),
        explode1(L,L1),
        append(L1,[93],L2),
        E = [91|L2].

explode1([],[]).
explode1([H|T],L) :-
        explode(H,L1),
        explode1(T,L2),
        append(L1,L2,L).

/* return a pseudo random number between 1 and R */
random(R,N) :-
        \+R=0,    % else div by 0 error
        ((retract(seed(X)),S=X) ;
         (statistics(runtime,[S,_]),
          integer(S),asserta(seed(S)))),
        N is (S mod R) +1,
        Newseed is (125 * S + 1) mod 4096,
        asserta(seed(Newseed)),!.

dynamic(seed/1).
seed(13).

/* remove duplicate entries from a list, maintaining original order */

removedups([],[]).
removedups([H|T],[H|L]) :- removetop(H,T,L1),
        removedups(L1,L).


/* remove all top level occurences of Element from a List */
removetop(_,[],[]).
removetop(E,[E|R],L) :- removetop(E,R,L).
removetop(E,[H|T],[H|T1]) :- removetop(E,T,T1).

/* setdif(S1,S2,S) removes all the members of S2 from S1 to yield S */

setdif([],_,[]).
setdif(A,[],A).
setdif(S,To_remove,Newset) :-
        length(S,S_length),
        length(To_remove,T_length),
        S_length > T_length,
        setdif1(S,To_remove,Newset).
setdif(S1,S2,NewS) :- setdif2(S1,S2,NewS).

setdif1(S,[],S).
setdif1(L,[H|T],L2) :- removetop(H,L,L1),
        setdif(L1,T,L2).

setdif2(S,[],S).
setdif2([],_,[]).
setdif2([H|T],Remove_list,L) :-
        member(H,Remove_list),
        setdif2(T,Remove_list,L).
setdif2([H|T],Remove_list,[H|L]) :-
        setdif2(T,Remove_list,L).


/* e.g. apply(append,[[1,2,3],[4,5,6],X]) will bind X to [1,2,3,4,5,6].
 */
apply(Functor,Arglist) :- X =.. [Functor|Arglist],X.




/*******************************************************************

                        FORMAT
  Two caveats:  Prolog cannot keep two files open at the same time
so amformat(t,'<some stuff>'[<sme args>]. goes to the currently open stream
which defaults to user at the beginning.  The moral of all this is that
you should close files with told. as soon as you are finished with them, 
and don't try to output to the user at the same time.

-Brad

Use:
        amformat(<filename>,<stuff>,<Args>).
<stuff> is the info to be printed surrounded by single quotes.
        It may also contain the following escape chars:
        
        ~a      Add the next value from <Args> to the output.

        ~l      Treat the next value on the <Args> as a
                predicate name.  List it to the output stream.

        ~n      Newline.

        ~s      skip white space, skips <cr>,spaces,tabs, to
                next char.  Use when you want to break up
                text in you source file.

<Args>  is a list containing Variables or atoms.  They are treated
        according to the escape chars in <stuff>.

Example:
        Fun = append,
        amformat(foo,'list the ~a on the next line ~n ~l',
                [function,Fun]).
would add "list the function on the next line
           append([],_1,_1).
           append([_1|_2],_3,[_1|_4]) :- append(_2,_3,_4)."
to the file foo.

*/

/* format/2 can be used if the current stream is desired */

amformat(Weird_atom,Args) :- amformat(t,Weird_atom,Args).
amformat(Stream,Atom,Args) :- format1(Stream,Atom,Args), !.

format1([],[],_).
format1([], Weird_big_atom, Args) :-
  tell(user),
  name(Weird_big_atom, String),
  formatprint(String,Args).
format1(t, Weird_big_atom, Args) :-    /* t is zeta syntax, and can be*/
  name(Weird_big_atom, String),       /* changed to any global */
  formatprint(String,Args).
format1(File, Weird_big_atom, Args) :-
  tell(File),
  name(Weird_big_atom,String),
  formatprint(String, Args).

formatprint([126, 97 | Rest_of_string],Args) :-  /* the case of ~a */
  first_or_nil(Args,An_arg,Rest_of_args),       /* a is for atom */
  write(An_arg),
  formatprint(Rest_of_string,Rest_of_args).
formatprint([126, 108 | Rest_of_string],Args) :-  /* this is ~l */
  first_or_nil(Args,An_arg,Rest_of_args),         /* l is for listing */
  listing(An_arg),
  formatprint(Rest_of_string,Rest_of_args).
formatprint([126, 110 | Rest_of_string],Args) :-  /* and ~n */
  nl,                                             /* n is for newline */
  formatprint(Rest_of_string,Args).
formatprint([126, 115, 31 | Rest_of_string],Args) :-  /* and ~s */
  remove_white_space(Rest_of_string, String),     /* s is for skip cr's and */
  formatprint(String,Args).                       /* other white space */
formatprint([126, 110 | Rest_of_string],Args) :-  /* and ~n */
  put(32), put(32),                               /* n is for newline */
  formatprint(Rest_of_string,Args).
formatprint([Letter | Rest_of_string],Args) :-
  put(Letter),
  formatprint(Rest_of_string,Args).
formatprint([],_).

/* maybe? */

prompt_and_read(Weird_atom, Args, Answer) :-
  format1([],Weird_atom,Args),ttyflush,
  myinput(Answer).



remove_white_space([32 | Rest], No_white) :-
  remove_white_space(Rest,No_white).
remove_white_space([9 | Rest], No_white) :-
  remove_white_space(Rest, No_white).
remove_white_space(No_white, No_white).

/* this does work and produces the file foo which is both human and
machine readable */

test(X) :-
  X1 is X + 1, X2 is X + 2, X3 is X + 3,
  amformat([],'~a testing foo ~n bar ~s baz ~a testing ~a', [X1, X2, X3]), nl,
  amformat(foo,'/* foo written ~n this is formatprint */ ~l ',[formatprint]),
  amformat(t,'~n /* this is test */ ~n  ~l ', [test]),
  told,
  amformat([],'~a testing ~a testing ~a', [X1, X2, X3]), nl,
  nl.


/* surely I'm not the only one who needs (car ()) -> (cdr ()) -> (). */

first_or_nil([],[],[]).
first_or_nil([H | T],H,T).

randomelement(L,E) :-
        length(L,N),
        random(N,R),
        nth(L,R,E).

break([H|T],Element,1,Restoflist):-Element=H,Restoflist=T,!.
break([H|T],Element,Index,Restoflist):-Newindex is Index-1,
                                         Restoflist=[H|Rest],
                                         break(T,Element,Newindex,Rest).
/* breaks a list into Element and the rest of the list. [H|T] is the list,
   Element is to be the Indexth element, and Restoflist is the list without
   the Indexth element.
*/

randombreak(List,Element,Restoflist):-
                                 length(List,Len),random(Len,An_index),
                                 break(List,Element,An_index,Restoflist),!.
/* breaks List into a random Element and the Restoflist. */

remove_random(L1,L2) :-
        length(L1,N),
        random(N,R),
        remove_nth(L1,R,L2).
remove_nth([],0,[]).
remove_nth([H|T],0,T).
remove_nth([H|T],N,[H|New]) :-
        N1 is N-1,
        remove_nth(T,N1,New).

