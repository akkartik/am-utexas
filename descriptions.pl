/* Here is a list of people who have implemented the heuristics so far.  
   adam 	= Adam Farquhar
   annanya	= Annanya ?  (ask adam)
   bruce	= Dr. Bruce Porter
   ken		= Ken Murray
   kim		= Kim Matocha
   marcos	= M.V. LaPolla
   martin	= Dr. Martin Purvis
   ray		= Ray Bareiss
   todd		= Todd Stock
*/

descr(h1,'Boost worth of recently referenced concepts',adam).
descr(h6,'C is interesting if refered to in interesting conjectures',martin).
descr(h12,'Fillin all blank facets -go get some coffee',adam).
descr(h14,'After dealing with C, boost Cons that use C',adam).
descr(h17,'C is interesting if C.conjecs has interesting entries',martin).
descr(h20,'C is interesting if its boundary corresponds another con', martin).
descr(h23,'C is interesting if it satisfies some rare predicate',martin).
descr(h28, 'Same idea as 114',marcos).
descr(h29,'Find exs of X by looking at exs of more gen cons',ken).
descr(h31,'Find exs of X by unfolding its definition',adam).
descr(h34a,'Find exs of X by looking at ops whose range is X',ken).
descr(h34b,'Find exs of X by looking at ops whose domain is X',ken).
descr(h36, 'h36',marcos).
descr(h40,'Find exs of X by looking at first cousins of X',ken).
descr(h43,'If X and Y share many examples, then create their intersection', adam).
descr(h44,'If there are very few exs of C, then generalize it',adam).
descr(h45,'If there are very many exs of C, then specialize it',adam).
descr(h46,'If there are no exs of C, then find some',adam).
descr(h50,'After filling in exs of C, check them',adam).
descr(h56,'If a gen of C has same exs as C, they may be the same',ken).
descr(h57,'If a spec of C has same exs as C, they may be the same',ken).
descr(h59a,'Check exs of C against the defn of C',anonymous).
descr(h59c,'Prune the exs slot of C to a size reflecting its worth',anonymous).
descr(h61a,'Move typical exs to as specific a con as possible',ken).
descr(h61b,'Move bnd exs to as specific a con as possible',ken).
descr(h89,'Generalize C by dropping conjuncts',bruce).
descr(h92,'Specialize C by dropping disjuncts',bruce).
descr(h110,'Make sure that no specs of S are the same',annanya).
descr(h111,'If a gen & spec of C have a common elem, they may be the same', annanya).
descr(h114, 'If C1 is a genl of C2 if C2 is a fenl of C3 ... if Ck is a genl of Cn then merge and increase the value of the highest value to begin with',marcos).
descr(h114a, 'Same idea as 114 with a few mods',marcos).
descr(h116,'Fillin in_dom_of by finding what can be run on C',annanya).
descr(h117,'Fillin in_ran_of by finding ops that yield Cs',annanya).
descr(h123,'Fillin exs of op by running it on domain exs',adam).
descr(h124,'Fillin dom_range of C by finding where C is defined',annanya).
descr(h174,'Create the composition FoG',ray).
descr(h180,'Fillin exs of FoG by using exs of F and G',ray).
descr(h183,'Check that FoG is different than F or G',ray).
descr(h199,'Coalesce C',annanya).
descr(h204,'Creating f-itself',annanya).
descr(h240,'Find exs of mult elem by repeating elems of no-mult-elem', annanya).
descr(h300,'Generalize concept definition by generalizing a predicate',todd).
descr(h301,'Specialize concept definition by specializing a predicate',kim).

descr(h402, 'If number of examples are between 5 and 30 incr worth',marcos).
descr(h407, 'If a concept is worthwhile then compose it with itself',marcos).
descr(h408, 'If a concept if worthwhile do it again',marcos).
descr(h409, 'If a concept is ww then invert it',marcos).
