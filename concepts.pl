  %break% con.pl          526016830   409   20    100644  7918      `
/* This is the concept definitions file. There are many changes and
comments through out this file. I have added many concepts that
were not here before and have done my best at adding their defn's. 

-mv LaPolla (marcos) 
*/

:- dynamic(frame/3).

frame(anything,[name],[anything]).
frame(anything,[spec],[any_concept,non_concept]).
frame(anything,[examples,typ],[anything,any_concept]).
frame(anything,[isas],[any_concept]).
frame(anything,[worth],[100]).
frame(anything,[suggest],[h1,h12]).

frame(any_concept,[name],['any concept']).
frame(any_concept,[defn,name],[any_concept_defn]).
frame(any_concept,[defn,arity],[1]).
frame(any_concept,[genl],[anything]).
frame(any_concept,[spec],[active,object]).
frame(any_concept,[examples,typ],[anything,any_concept,object,active]).
frame(any_concept,[isas],[anything,any_concept]).
frame(any_concept,[worth],[100]).
frame(any_concept,[suggest],[h14]).
frame(any_concept,[int],[h6,h17,h20,h23]).
frame(any_concept,[examples,fillin],[h29,h31,h34a,h34b,h40,h174]).
frame(any_concept,[examples,suggest],[h44,h45,h46,h50]).
frame(any_concept,[examples,check],[h56,h59a,h59c,h57,h61a,h61b]).
frame(any_concept,[genl,fillin],[h300,h89]).
frame(any_concept,[spec,fillin],[h301,h92]).
frame(any_concept,[genl,check],[h111,h110]).
frame(any_concept,[spec,check],[h110,h111]).
frame(any_concept,[conjecs,fillin],[183]).

frame(object,[name],[object]).
frame(object,[spec],[atom,struct,conjecs]).
frame(object,[genl],[any_concept]).
frame(object,[in_dom_of],[object_equality,set_insert,bag_insert,set_delete,bag_delete]).
frame(object,[examples,fillin],[h31]).
frame(object,[worth],[100]).

frame(active,[name],[active]).
frame(active,[genl],[any_concept]).
frame(active,[spec],[operation,predicate]).
frame(active,[isas],[any_concept]).
frame(active,[in_dom_of],[coalesce,compose]).
frame(active,[in_ran_of],[coalesce,compose]).
frame(active,[worth],[100]).
frame(active,[in_dom_of,fillin],[h116]).
frame(active,[in_ran_of,fillin],[h117]).
frame(active,[dom_range,fillin],[h124]).


frame(operation,[name],[operation]).
frame(operation,[examples,typ],[compose,insert,member,delete,coalesce,length]).
frame(operation,[spec],[compose,insert,member,delete,coalesce,length]).
frame(operation,[genl],[active]).
frame(operation,[examples,fillin],[h123]).
frame(operation,[worth],[100]).

frame(predicate,[name],[predicate]).
frame(predicate,[genl],[active]).
frame(predicate,[examples,typ],[object_equality]).
frame(predicate,[worth],[100]).
frame(predicate,[defn,name],[predicate_defn]).

frame(coalesce,[name],[coalesce]).
frame(coalesce,[genl],[operation]).
frame(coalesce,[isas],[operation]).
frame(coalesce,[dom_range],[[active,active],[operation,operation],
        [predicate,predicate]]).
frame(coalesce,[worth],[300]).

frame(insert,[name],[insert]).
frame(insert,[isas],[operation]).
frame(insert,[spec],[set_insert,bag_insert]).
frame(insert,[genl],[operation]).
frame(insert,[dom_range],[[object,struct,struct]]).
frame(insert,[worth],[100]).
frame(insert,[defn,name],[insert_defn]).

frame(delete,[name],[delete]).
frame(delete,[defn,name],[delete_defn]).
frame(delete,[isas],[operation]).
frame(delete,[spec],[set_delete,bag_delete]).
frame(delete,[genl],[operation]).
frame(delete,[dom_range],[[object,struct,struct]]).
frame(delete,[worth],[100]).


frame(member,[name],[member]).
frame(member,[defn,name],[member_defn]).
frame(member,[isas],[operation]).
frame(member,[spec],[set_member,bag_member]).
frame(member,[genl],[operation]).
frame(member,[dom_range],[[struct,object]]).
frame(member,[worth],[100]).


frame(atom,[name],[atom]).
frame(atom,[genl],[object]).
frame(atom,[examples,typ],[a,b,c,d,e,f,g]).
frame(atom,[defn,name],[atom_defn]).
frame(atom,[defn,arity],[1]).

frame(struct,[name],[struct]).
frame(struct,[spec],[bag,set]).
frame(struct,[genl],[object]).
frame(struct,[in_dom_of],[insert,delete,member]).
frame(struct,[in_ran_of],[insert,delete]).
frame(struct,[defn,name],[struct_defn]).
%the above get 10 lenats worth
frame(struct,[defn,arity],[1]).
frame(struct,[worth],[200]).

frame(conjecs,[name],[conjecs]).
frame(conjecs,[genl],[object]).
frame(conjecs,[worth],[300]).


frame(set,[name],[set]).
/* NB: a set is NOT a spec of bag frame(set,[genl],[bag]). */
frame(set,[genl],[struct]).
frame(set,[examples,typ],[[a],[a,b],[a,b,[b,a]],[[[c]]]]).
frame(set,[examples,bnd],[[]]).
frame(set,[in_dom_of],
       [set_insert,set_delete]).
frame(set,[in_ran_of],
       [set_insert,set_delete]).
frame(set,[worth],[400]).
frame(set,[defn,name],[set_defn]).
frame(set,[defn,arity],[1]).

frame(bag,[name],[bag]).
frame(bag,[genl],[struct]).
frame(bag,[defn,name],[bag_defn]).
frame(bag,[defn,arity],[1]).
frame(bag,[in_dom_of],[bag_equal,bag_insert,bag_delete,bag_member]).
frame(bag,[in_ran_of],[bag_insert,bag_delete,bag_equal]).
frame(bag,[worth],[400]).

frame(bag_equal,[name],[bag_equality]).
frame(bag_equal,[genl],[object_equality]).
frame(bag_equal,[defn,name],[bag_equal_defn]).
frame(bag_equal,[defn,arity],[2]).
frame(bag_equal,[dom_range],[[bag,bag]]).
frame(bag_equal,[alg],[bag_equal_defn]).
frame(bag_equal,[worth],[100]).


frame(bag_member,[name],[bag_memberity]).
frame(bag_member,[genl],[member]).
frame(bag_member,[defn,name],[bag_member_defn]).
frame(bag_member,[defn,arity],[2]).
frame(bag_member,[dom_range],[[bag,object]]).
frame(bag_member,[alg],[bag_member_defn]).
frame(bag_member,[worth],[100]).


frame(bag_insert,[name],[bag_insert]).
frame(bag_insert,[genl],[insert]).
frame(bag_insert,[defn,name],[bag_insert_defn]).
frame(bag_insert,[defn,arity],[3]).
frame(bag_insert,[dom_range],[[object,bag,bag]]).
frame(bag_insert,[alg],[bag_insert_defn]).
frame(bag_insert,[worth],[100]).


frame(bag_delete,[name],[bag_delete]).
frame(bag_delete,[genl],[delete]).
frame(bag_delete,[defn,name],[bag_delete_defn]).
frame(bag_delete,[defn,arity],[3]).
frame(bag_delete,[dom_range],[[object,bag,bag]]).
frame(bag_delete,[alg],[bag_delete_defn]).
frame(bag_delete,[worth],[100]).



frame(object_equality,[name],[object_equality]).
frame(object_equality,[genl],[predicate]).
frame(object_equality,[isas],[predicate]).
frame(object_equality,[defn,name],[object_equality_defn]).
frame(object_equality,[dom_range],[[object,object,true_false],
                     [strut,struct,true_false]]).
frame(object_equality,[algorithms],[object_equality_alg]).
frame(object_equality,[conjecs],[[object_equality_code,X,X],
                                 [structs_not_in,dom_range],
                                 [same_as,object_equality,
                                          object_equality,true]]).

frame(object_equality,[worth],[200]).

frame(set_insert,[name],[set_insert]).
frame(set_insert,[defn,name],[set_insert_defn]).
frame(set_insert,[dom_range],[[object,set,set]]).
frame(set_insert,[alg],[set_insert_alg]).       
frame(set_insert,[genl],[insert]).
frame(set_insert,[worth],[100]).

frame(set_delete,[name],[set_delete]).
frame(set_delete,[defn,name],[set_delete_defn]).
frame(set_delete,[dom_range],[[object,set,set]]).
frame(set_delete,[alg],[set_delete_alg]).
frame(set_delete,[genl],[delete]).
frame(set_delete,[worth],[100]).

frame(set_member,[name],[set_member]).
frame(set_member,[defn,name],[set_member_defn]).
frame(set_member,[dom_range],[[struct,object]]).
frame(set_member,[alg],[set_member_alg]).
frame(set_member,[isas],[operation]).
frame(set_member,[genl],[member]).
frame(set_member,[worth],[100]).

frame(compose,[name],[compose]).
%frame(compose,[defn,name],[compose_defn]).
%frame(compose,[alg],[compose_alg]).
frame(compose,[genl],[operation]).
frame(compose,[examples,fillin],[h174]).
frame(compose,[examples,check],[h183]).
frame(compose,[dom_range],[[active,active,active],
                           [operation,active,operation],
                           [predicate,active,predicate],
                           [relation,relation,relation]]).
frame(compose,[genl],[operation]).
frame(compose,[isas],[operation]).
frame(compose,[worth],[300]).

frame(length,[name],[length]).
frame(length,[defn,name],[length_defn]).
frame(length,[alg],[length_defn]).
frame(length,[dom_range],[[struct,struct],[set,struct]]).
frame(length,[genl],[operation]).
frame(length,[isas],[operation]).
frame(length,[worth],[300]).

%from here on out these are  new concepts -marcos

frame(bag_diff,[name],[bag_diff]).
frame(bag_diff,[defn,name],[bag_diff_defn]).
frame(bag_diff,[dom_range],[[bag,bag, bag]]).
frame(bag_diff,[worth],[100]).
frame(bag_diff,[genl],[difference]).
frame(bag_diff,[defn,arity],[3]).

frame(difference,[name],[difference]).
frame(difference,[isas],[operation]).
frame(difference,[dom_range],[[struct,struct,struct]]).
frame(difference,[spec],[set_diff,bag_diff,list_diff,oset_diff]).
frame(difference,[worth],[100]).
frame(difference,[defn,name],[difference_defn]).

frame(set_diff,[name],[set_diff]).
frame(set_diff,[genl],[difference]).
frame(set_diff,[worth],[100]).
frame(set_diff,[dom_range],[[set,set,set]]).
frame(set_diff,[defn,name],[set_diff_defn]).

frame(list_diff,[name],[list_diff]).
frame(list_diff,[dom_range],[[list,list,list]]).
frame(list_diff,[worth],[100]).
frame(list_diff,[genl],[difference]).
frame(list_diff,[defn,name],[list_diff_defn]).

frame(oset_diff,[name],[oset_diff]).
frame(oset_diff,[name],[oset_difference]).
frame(oset_diff,[worth],[100]).
frame(oset_diff,[genl],[difference]).
frame(oset_diff,[dom_range],[[oset,oset,oset]]).
frame(oset_diff,[defn,name],[oset_diff_defn]).

frame(oset,[name],[oset]).
frame(oset,[worth],[400]).
frame(oset,[genl],[ordered_struct,no_mult_elements_struct]).
frame(oset,[in_dom_of],[oset_union,oset_intersect,oset_diff,
	oset_insert,oset_delete]).
frame(oset,[in_ran_of],[oset_union,oset_intersect,oset_diff,
	oset_insert,oset_delete]).
frame(oset,[defn,name],[oset_defn]).

frame(bag_intersect,[name],[bag_intersect]).
frame(bag_intersect,[genl],[intersect]).
frame(bag_intersect,[worth],[100]).
frame(bag_intersect,[dom_range],[[bag,bag,bag]]).
frame(bag_intersect,[defn,name],[bag_intersect_defn]).

frame(bag_union,[name],[bag_union]).
frame(bag_union,[genl],[union]).
frame(bag_union,[dom_range],[[bag,bag,bag]]).
frame(bag_union,[worth],[100]).
frame(bag_union,[defn,name],[bag_union_defn]).

frame(union,[name],[union]).
frame(union,[isas],[operation]).
frame(union,[spec],[set_union,bag_union,list_union,oset_union]).
frame(union,[worth],[100]).
frame(union,[dom_range],[[struct,struct,struct]]).
frame(union,[defn,name],[union_defn]).

frame(canonize,[name],[canonize]).
frame(canonize,[dom_range],[[predicate,predicate,operation]]).
frame(canonize,[genl],[operation]).
frame(canonize,[isas],[operation]).
frame(canonize,[worth],[200]).
%frame(canonize,[fillin],
%frame(canonize,[suggest],
%frame(canonize,


frame(list_union,[name],[list_union]).
frame(list_union,[dom_range],[[list,list,list]]).
frame(list_union,[genl],[union]).
frame(list_union,[worth],[100]).
frame(list_union,[defn,name],[list_union_defn]).

frame(list_intersect,[name],[list_intersect]).
frame(list_intersect,[dom_range],[[list,list,list]]).
frame(list_intersect,[genl],[intersect]).
frame(list_intersect,[worth],[100]).
frame(list_intersect,[defn,name],[list_intersect_defn]).

frame(intersect,[name],[intersect]).
frame(intersect,[dom_range],[[struct,struct,struct]]).
frame(intersect,[isas],[operation]).
frame(intersect,[spec],[set_intersect,bag_intersect,list_intersect,
	oset_intersect]).

frame(intersect,[worth],[100]).
frame(intersect,[defn,name],[intersect_defn]).

frame(set_union,[name],[set_union]).
frame(set_union,[dom_range],[[set,set,set]]).
frame(set_union,[genl],[union]).
frame(set_union,[worth],[100]).
frame(set_union,[defn,name],[set_union_defn]).

frame(set_intersect,[name],[set_intersect]).
frame(set_intersect,[genl],[intersect]).
frame(set_intersect,[dom_range],[[set,set,set]]).
frame(set_intersect,[worth],[100]).
frame(set_intersect,[defn,name],[set_intersect_defn]).

frame(oset_union,[name],[oset_union]).
frame(oset_union,[dom_range],[[oset,oset,oset]]).
frame(oset_union,[genl],[union]).
frame(oset_union,[worth],[100]).
frame(oset_union,[defn,name],[oset_union_defn]).

frame(oset_intersect,[name],[oset_intersect]).
frame(oset_intersect,[genl],[intersect]).
frame(oset_intersect,[worth],[100]).
frame(oset_intersect,[dom_range],[[oset,oset,oset]]).
frame(oset_intersect,[defn,name],[oset_intersect_defn]).

frame(oset_insert,[name],[oset_insert]).
frame(oset_insert,[dom_range],[[anything,oset,oset]]).
frame(oset_insert,[genl],[insert]).
frame(oset_insert,[worth],[100]).
frame(oset_insert,[defn,name],[oset_insert_defn]).

frame(oset_delete,[name],[oset_delete]).
frame(oset_delete,[dom_range],[[anything,oset,oset]]).
frame(oset_delete,[genl],[delete]).
frame(oset_delete,[worth],[100]).
frame(oset_delete,[defn,name],[oset_delete_defn]).

frame(ordered_struct,[name],[ordered_struct]).
frame(ordered_struct,[spec],[oset,list]).
frame(ordered_struct,[genl],[struct]).
frame(ordered_struct,[worth],[200]).
%frame(ordered_struct,[fillin],[]).
%frame(ordered_struct,[check],
%frame(ordered_struct,[interest],
frame(ordered_struct,[defn,name],[ordered_struct_defn]).

frame(no_mult_elements_struct,[name],[no_mult_elements_struct]).
frame(no_mult_elements_struct,[spec],[set,oset]).
frame(no_mult_elements_struct,[genl],[struct]).
frame(no_mult_elements_struct,[worth],[200]).
frame(no_mult_elements_struct,[defn,name],
	[]).

frame(empty_struct,[name],[empty_struct]).
frame(empty_struct,[genl],[struct]).
frame(empty_struct,[worth],[100]).
frame(empty_struct,[defn,name],[empty_struct_defn]).



frame(nonempty_struct,[name],[nonempty_struct]).
frame(nonempty_struct,[genl],[struct]).
frame(nonempty_struct,[worth],[100]).
frame(nonempty_struct,[in_ran_of],[insert]).
frame(nonempty_struct,[defn,name],[nonempty_struct_defn]).


frame(list,[name],[list]).
frame(list,[spec],[ordered_pairs]).
frame(list,[genl],[ordered_struct,multiple_elements_struct]).
frame(list,[worth],[400]).
frame(list,[in_domain_of],[list_union,list_intersect,list_diff,list_insert,
	list_delete]).
frame(list,[in_ran_of],[list_union,list_intersect,list_diff,list_insert,
	list_delete]).
frame(list,[defn,name],[list_defn]).


frame(ordered_pairs,[name],[ordered_pairs]).
frame(ordered_pairs,[in_dom_of],[reverse_ord_pair]).
frame(ordered_pairs,[in_ran_of],[reverse_ord_pair]).
frame(ordered_pairs,[genl],[list]).
frame(ordered_pairs,[worth],[200]).
frame(ordered_pairs,[defn,name],[ordered_pairs_defn]).


frame(list_insert,[name],[list_insert]).
frame(list_insert,[dom_range],[[anything,list,list]]).
frame(list_insert,[genl],[insert]).
frame(list_insert,[worth],[100]).
frame(list_insert,[defn,name],[list_insert_defn]).

frame(multiple_elements_struct,[name],[multiple_elements_struct]).
frame(multiple_elements_struct,[spec],[list,bag]).
frame(multiple_elements_struct,[worth],[200]).
frame(multiple_elements_struct,[genl],[struct]).
frame(multiple_elements_struct,[defn,name],[]).

frame(first_element,[name],[first_element]).
frame(first_element,[isas],[operation]).
frame(first_element,[worth],[100]).
frame(first_element,[dom_range],[[ordered_struct,anything]]).
frame(first_element,[defn,name],[first_element_defn]).

frame(last_element,[name],[last_element]).
frame(last_element,[isas],[operation]).
frame(last_element,[worth],[100]).
frame(last_element,[dom_range],[[ordered_struct,anything]]).
frame(last_element,[defn,name],[last_element_defn]).

/* I have a large problem with the next few concepts, all the
ones with constant-X. My problem is this: are these concepts'
defn's to be used to check other concepts so that we can see
if they produce constant values or are they predicates which produce
constant values? I am going to assume that for the defn they are
like lenat's defn's. That is constant_false takes anything and
gives back false, it is therefore not a category. But I am also going
to include a check function that will weakly check to see if 
a concept is a constant function! -marcos

I have figured out the problem: constant_true and false are examples
of constant_predicate and not really specializations, or at least as
much as any specialization is an example or any example a specialization
by definition. However, because they are not  categories of concepts
as so much of these concepts are, they cannot have examples themselves
but only bothers. For example, constant true is an example of the
concept funtions-which-return-true just as the predicate Vx:large(x) &
~large(x), is an example of funtions-which-return-true but is it an
example of true? Prehaps of truth but true? -marcos
*/

frame(constant_false,[name],[constant_false]).
frame(constant_false,[dom_range],[[anything,anything,false],
	[anything,anything,true_false]]).
frame(constant_false,[genl],[constant_prediate]).
frame(constant_false,[worth],[100]).
frame(constant_false,[defn,name],[constant_false_defn]).
frame(constant_false,[examples,check],[constant_h_1]).

frame(constant_predicate,[name],[constant_predicate]).
frame(constant_predicate,[dom_range],[[anything,anything,true_false]]).
frame(constant_predicate,[isas],[predicate]).
frame(constant_predicate,[spec],[constant_true,constant_false]).
frame(constant_predicate,[worth],[100]).
/*rather than use lenat's idea of constant_predicate_defn,
i.e. check the constant_predicate.EXs slot, I invent one.
*/
frame(constant_predicate,[defn,name],[constant_predicate_defn]).

frame(constant_true,[name],[constant_true]).
frame(constant_true,[dom_range],[[anything,anything,true],[anything,anything,true_false]]).
frame(constant_true,[genl],[constant_predicate]).
frame(constant_true,[worth],[100]).
frame(constant_true,[defn,name],[constant_true_defn]).
frame(constant_true,[examples,check],[constant_h_2]).
frame(constant_true,[defn,arity],[1]).

frame(list_delete,[name],[list_delete]).
frame(list_delete,[dom_range],[[anything,list,list]]).
frame(list_delete,[genl],[delete]).
frame(list_delete,[worth],[100]).
frame(list_delete,[defn,name],[list_delete_defn]).

frame(first,[name],[first]).
frame(first,[genl],[list_delete]).
frame(first,[dom_range],[[list,list,list],[bag,bag,bag],[struct,struct,struct]]).

frame(first,[worth],[200]).
frame(first,[defn,name],[first]).
%all of the prolog predicates that are being represented here, built in
%or no, have their name as their defn name -marcos

frame(rest,[name],[rest]).
frame(rest,[dom_range],[[list,list,list],[bag,bag,bag],[struct,struct,struct],
  	[list,list,empty_struct],[bag,bag,empty_struct],[struct,struct,empty_struct]]).
frame(rest,[genl],[list_delete]).
frame(rest,[worth], [200]).
frame(rest,[defn,name],[rest_defn]).

frame(reverse_ord_pair,[name],[reverse_ord_pair]).
frame(reverse_ord_pair,[isas],[operation]).
frame(reverse_ord_pair,[dom_range],[[ordered_pairs,ordered_pairs]]).
frame(reverse_ord_pair,[worth],[100]).
frame(reverse_ord_pair,[defn,name],[reverse_ord_pair_defn]).


frame(identity,[name],[identity]).
frame(identity,[dom_range],[[anything,anything],[object,object],
	[struct,struct],[active,active]]).
frame(identity,[conjecs],['identity, restricted to objects, is the same as object-equality']).
frame(identity,[genl],[projection1,projection2]).
frame(identity,[worth],[100]).
frame(identity,[defn,name],[identity_defn]).


frame(projection1,[name],[projection1]).
frame(projection1,[dom_range],[[X,anything,anything,X]]).
frame(projection1,[spec],[identity]).
frame(projection1,[isas],[operation]).
frame(projection1,[worth],[100]).
frame(projection1,[defn,name],[]).

frame(projection2,[name],[projection2]).
frame(projection2,[dom_range],[[anything,X,anything,X]]).
frame(projection2,[spec],[identity]).
frame(projection2,[isas],[operation]).
frame(projection2,[worth],[200]).
frame(projection2,[defn,name],[]).

frame(invert_an_op,[name],[invert_an_op]).
frame(invert_an_op,[isas],[operation]).
frame(invert_an_op,[worth],[300]).
frame(invert_an_op,[dom_range],[[operation,operation],[operation,inverted_op]]).
frame(invert_an_op,[defn,name],[invert_op_defn]).

frame(inverted_op,[name],[inverted_op]).
frame(inverted_op,[genl],[operation]).
frame(inverted_op,[worth],[200]).
frame(inverted_op,[in_dom_of],[invert]).
frame(inverted_op,[in_ran_of],[invert]).
frame(inverted_op,[defn,name],[inverted_op_defn]).

frame(logical_combination,[name],[logical_combination]).
frame(logical_combination,[genl],[relation]).
frame(logical_combination,[worth],[200]).
frame(logical_combination,[defn,name],[]).

frame(parallel_join,[name],[parallel_join]).
frame(parallel_join,[genl],[parallel_join2]).
frame(parallel_join,[worth],[100]).
frame(parallel_join,[dom_range],[[type_of_struct,operation,operation]]).
frame(parallel_join,[defn,name],[]).

frame(parallel_join2,[name],[parallel_join2]).
frame(parallel_join2,[spec],[parallel_join]).
frame(parallel_join2,[isas],[operation]).
frame(parallel_join2,[worth],[100]).
frame(parallel_join2,[dom_range],[[type_of_struct,type_of_struct,operation]]).
frame(parallel_join2,[defn,name],[]).

frame(parallel_replace,[name],[parallel_replace]).
frame(parallel_replace,[genl],[parallel_replace2]).
frame(parallel_replace,[worth],[100]).
frame(parallel_replace,[dom_range],[[type_of_struct,operation,operation]]).
frame(parallel_replace,[defn,name],[]).

frame(parallel_replace2,[name],[parallel_replace2]).
frame(parallel_replace2,[spec],[parallel_replace]).
frame(parallel_replace2,[isas],[operation]).
frame(parallel_replace2,[worth],[100]).
frame(parallel_replace2,[dom_range],[[type_of_struct,type_of_struct,operation]]).
frame(parallel_replace2,[defn,name],[]).

frame(relation,[name],[relation]).
frame(relation,[genl],[active]).
frame(relation,[spec],[logical_combination]).
frame(relation,[worth],[100]).
frame(relation,[defn,name],[]).

frame(repeat,[name],[repeat]).
frame(repeat,[genl],[repeat2]).
frame(repeat,[worth],[100]).
frame(repeat,[dom_range],[[type_of_struct,operation,operation]]).
frame(repeat,[defn,name],[]).

frame(repeat2,[name],[repeat2]).
frame(repeat2,[spec],[repeat]).
frame(repeat2,[isas],[operation]).
frame(repeat2,[worth],[100]).
frame(repeat2,[dom_range],[[type_of_struct,type_of_struct,operation,operation]]).
frame(repeat2,[defn,name],[]).

frame(restrict,[name],[restrict]).
frame(restrict,[isas],[operation]).
frame(restrict,[worth],[200]).
frame(restrict,[dom_range],[[active,active],[operation,operation],[predicate,
		predicate]]).
frame(restrict,[defn,name],[restrict_defn]).

frame(struct_of_struct,[name],[struct_of_struct]).
frame(struct_of_struct,[genl],[object]).
frame(struct_of_struct,[in_dom_of],[insert,delete,member,empty,nonempty,difference,union,intersect,parallel-replace2,parallel_join2,repeat2]).
frame(struct_of_struct,[in_ran_of],[insert,delete,difference,union,intersect]).
frame(struct_of_struct,[worth],[200]).
frame(struct_of_struct,[spec],[ord_struct,empty_struct,unord_struct,nonempty_struct]).
frame(struct_of_struct,[defn,name],[struct_of_struct_defn]).

frame(truth_value,[name],[truth_value]).
frame(truth_value,[genl],[atom]).
frame(truth_value,[worth],[100]).
frame(truth_value,[defn,name],[]).

frame(unord_struct,[name],[unord_struct]).
frame(unord_struct,[genl],[struct]).
frame(unord_struct,[spec],[set,bag]).
frame(unord_struct,[worth],[200]).
frame(unord_struct,[defn,name],[]).
