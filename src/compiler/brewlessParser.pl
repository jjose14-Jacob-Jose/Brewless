program_eval(t_prog(B)) :- block_eval(B, [], OutEnv).

block_eval(t_block(C), Env, OutEnv) :- command_eval(C, Env, OutEnv).
block_eval(t_block(C, B), Env, OutEnv) :- command_eval(C, Env, Env1), command_eval(B, Env1, OutEnv).

command_eval(D, Env, OutEnv) :- declaration_eval(D, Env, OutEnv).
command_eval(A, Env, OutEnv) :- assign_eval(A, Env, OutEnv).
command_eval(C, Env, OutEnv) :- conditional_eval(C, Env, OutEnv).
command_eval(F, Env, OutEnv) :- for_eval(F, Env, OutEnv).
command_eval(W, Env, OutEnv) :- while_eval(W, Env, OutEnv).
command_eval(T, Env, OutEnv) :- tern_eval(T, Env, OutEnv).
command_eval(P, Env, OutEnv) :- print_eval(P, Env, OutEnv).

declaration_eval(t_dec(T,I), Env, OutEnv) :- type_eval(T, Env, Env1), identifier_eval(I, Env1, OutEnv).
declaration-eval(t_dec(T,A), Env, OutEnv) :- type_eval(T, Env, Env1), assign_eval(A, Env1, OutEnv).

type_eval(t_type(I), Env, OutEnv) :- integer_eval(I, Env, OutEnv).
type_eval(t_type(S), Env, OutEnv) :- string_eval(S, Env, OutEnv).
type_eval(t_type(B), Env, OutEnv) :- boolean_eval(B, Env, OutEnv).

assign_eval(t_assign(I,E), Env, OutEnv) :- identifier_eval(I, Env, Env1), expression_eval(E, Env1, OutEnv).

expression_eval(I, Env, OutEnv) :- identifier_eval(I, Env, OutEnv).
expression_eval(I, Env, OutEnv) :- integer_eval(I, Env, OutEnv).
expression_eval(S, Env, OutEnv) :- string_eval(S, Env, OutEnv).
expression_eval(B, Env, OutEnv) :- boolean_eval(B, Env, OutEnv).
expression_eval(t_plus(A,B), Env, OutEnv) :- expression_eval(A, Env, Env1), term_eval(B, Env1, OutEnv).
expression_eval(t_minus(A,B), Env, OutEnv) :- expression_eval(A, Env, Env1), term_eval(B, Env1, OutEnv).
expression_eval(A, Env, OutEnv) :- expression_eval(A, Env, OutEnv).

term_eval(t_times(A, B), Env, OutEnv) :- term_eval(A, Env, Env1), paren_eval(B, Env1, OutEnv).
term_eval(t_divide(A, B), Env, OutEnv) :- term_eval(A, Env, Env1), paren_eval(B, Env1, OutEnv).
term_eval(A, Env, OutEnv) :- paren_eval(A, Env, OutEnv).

paren_eval(t_paren(A), Env, OutEnv) :- expression_eval(A, Env, OutEnv).
paren_eval(A, Env, OutEnv) :- value_eval(A, Env, OutEnv).

value_eval(A, Env, OutEnv) :- identifier_eval(A, Env, OutEnv).
value_eval(A, Env, OutEnv) :- integer_eval(A, Env, OutEnv).

integer_eval(I, Env, OutEnv) :- [I], {is_of_type(integer, I)}.

identifier_eval(L, Env, OutEnv) :- letter_eval(L, Env, OutEnv).
%Might want to change how we do our identifiers.
%letter_eval(A, Env, OutEnv) :- 

bool_eval(t_bool(true), _, _, true). 
bool_eval(t_bool(false), _, _, false).
bool_eval(t_bool(E1, '==', E2), Env, OutEnv, true) :- E1 =:= E2.
bool_eval(t_bool(E1, '==', E2), Env, OutEnv, false) :- E1 =/= E2.
bool_eval(t_bool(not, B), Env, OutEnv, Result):- bool_eval(B, Env, OutEnv, Temp), bool_not(Temp, Result).
bool_eval(t_bool(B1, and, B2), Env, OutEnv, Result) :- bool_eval(B1, Env, OutEnv, Temp1), bool_eval(B2, Env, OutEnv, Temp2), bool_check(Temp1, and, Temp2, Result). 
bool_eval(t_bool(B1, or, B2), Env, OutEnv, Result) :- bool_eval(B1, Env, OutEnv, Temp1), bool_eval(B2, Env, OutEnv, Temp2), bool_check(Temp1, or, Temp2, Result). 
bool_eval(t_ident(I1, '<', I2), Env, OutEnv, true) :- identifier_eval(I1, Env, OutEnv, Temp1), identifier_eval(I2, Env, OutEnv, Temp2), Temp1 < Temp2.
bool_eval(t_ident(I1, '<', I2), Env, OutEnv, false) :- identifier_eval(I1, Env, OutEnv, Temp1), identifier_eval(I2, Env, OutEnv, Temp2), Temp1 >= Temp2.
bool_eval(t_ident(I1, '<=', I2), Env, OutEnv, true) :- identifier_eval(I1, Env, OutEnv, Temp1), identifier_eval(I2, Env, OutEnv, Temp2), Temp1 <= Temp2.
bool_eval(t_ident(I1, '<=', I2), Env, OutEnv, false) :- identifier_eval(I1, Env, OutEnv, Temp1), identifier_eval(I2, Env, OutEnv, Temp2), Temp1 > Temp2.
bool_eval(t_ident(I1, '>', I2), Env, OutEnv, true) :- identifier_eval(I1, Env, OutEnv, Temp1), identifier_eval(I2, Env, OutEnv, Temp2), Temp1 > Temp2.
bool_eval(t_ident(I1, '>', I2), Env, OutEnv, false) :- identifier_eval(I1, Env, OutEnv, Temp1), identifier_eval(I2, Env, OutEnv, Temp2), Temp1 <= Temp2.
bool_eval(t_ident(I1, '>=', I2), Env, OutEnv, true) :- identifier_eval(I1, Env, OutEnv, Temp1), identifier_eval(I2, Env, OutEnv, Temp2), Temp1 >= Temp2.
bool_eval(t_ident(I1, '>=', I2), Env, OutEnv, false) :- identifier_eval(I1, Env, OutEnv, Temp1), identifier_eval(I2, Env, OutEnv, Temp2), Temp1 < Temp2.

bool_not(true, false).
bool_not(false, true).

bool_check(true , and, true  , true).
bool_check(true , and, false , false).
bool_check(false , and, true  , false).
bool_check(false , and, false , false).
bool_check(true , or , true  , true).
bool_check(true , or , false , true).
bool_check(false , or , true  , true).
bool_check(false , or , false , false).


%Taken From Assign 3 Semantics, needs editing
lookup(Identifier, [(Identifier, Number) | _], Number).
lookup(Identifier, [_ | T], Number):- lookup(Identifier, T, Number).

update(Identifier, [(Identifier, _) | T], Number, [(Identifier, Number) | T]).
update(Identifier, [H | T], Number, [H | Env]):- update(Identifier, T, Number, Env).

setEnv((Identifier, Number), Env, Result):- EnvTemp = [(Identifier, Number)], append(Env, EnvTemp, Result).
