# SAT_propositional_logical_inference

SAT_propositional_logical_inference Obtaining the satisfaction of a knowledge base by a different procedure from the conventional one.

Currently, it is usually passed from an expression in FBF (well-formed formula with literals separated by the connectors =>, <=>, v, ^, ¬ and parentheses) to an expression in FNC composed of clauses (literals separated by the or v operator) joined by the and ^ operator, following many steps: substitution of expressions joined by the => connectors by their equivalent  Boolean expression,  application of Morgan's laws ... etc.

The method followed in  this application is:

A new variable PPNIL is “created”, which the application considers as NIL but which prevents Lisp from considering it as NIL with the consequences it has in handling lists of considering it as an end of list.

The values of each literal are substituted for T or PPNIL  as they correspond to True or Nil in the composition of literals to be tested.
The following rules apply.

Connector separated literals =>
Expressions that are True: T => T, PPNIL => T, PPNIL => PPNIL and Nil the rest.

Connnector separated literals <=>
Expressions that are True: T<=>T PPNIL<=>PPNIL and Nil the rest.

In the case of expressions joined by the v (or) connector, it is enough that one of the elements is T for it to consider the expression a T.

In the case of connector ^ (and), it is enough that one of the variables is PPNIL for the expression to be considered false, that is, PPNIL.

All this is done in the function valora-proposición-reducida-T-NIL (propos) function that determines whether a statement reduced to values T and PPNIL is true or false. As soon as it finds a list, it makes a recursive call to itself to obtain the T or PPNIL evaluation of the list. 
The function encuentra-modelos-p (kb) is the most important since it not only determines if the expression Kb that is passed to it is true or false, but it also gives the list of variables (interpretations) with T or PPNIL values that satisfy them . To do this, it uses the function extrae-simbolos( kb), which generates a list with all the possible values of the variables of the expression; genera-lista-interpretaciones, which generates all the possible combinations of variable values that can be given with T and PPNIL; and explora-todas-interpretaciones ( listB kb), that successively calls interpretacion-modelo-p to check if it meets the interpretation of variable values to be passed.

It is a "brute force" calculation system, it checks all the possibilities of assigning variables, so it is slower than systems that use DPLL (https://github.com/ablanco1950/DPLL_propositional_logical_inference)
By using string handling functions in Lisp, the slowness is increased.

Another notable function is reduce-T-NIL-proposicion (propos lst-reductor) function that receives expressions and converts variables to T or PPNIL values instead of variables. Use string handling functions in Lisp. If there is a list, it is called recursively to see the values in T or PPNIL of each of the variables. It uses the ls-reductor list that assigns each variable a T or PPNIL value

TESTS:

The tests have been implemented based on the examples that appear in the README.md, section 7, of https://github.com/bertuccio/inferencia-logica-proposicional

Also based on the examples that appear in a link to netlogo that appears in http://www.cs.us.es/~fsancho/?e=120.

Requirements: Allegro CL 10.1 Free Express Edition

References:

https://github.com/bertuccio/inferencia-logica-proposicional by Adrián Lorenzo Mateo (Bertuccio) who uses material from the Artificial Intelligence practices at the Higher Polytechnic School of the Autonomous University of Madrid. Informatics Engineering.
http://www.cs.us.es/~fsancho/?e=120 by Fernando Sancho Caparrini. Higher Technical School of Computer Engineering of the University of Seville. https://github.com/ablanco1950/DPLL_propositional_logical_inference
