/*If C1 is a genl of C2 if C2 is a fenl of C3 ... if Ck is a genl of Cn then
merge and increase the value of the highest value to begin with*/

/* h114_it is in h114a.pl
*/


h114(C):- assertz(counter(0)),h114_it(C,C,0,[]).


