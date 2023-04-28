program_eval(t_prog(B), OutEnv) :- block_eval(B, [], OutEnv).

block_eval(t_block(C), Env, OutEnv) :- command_eval(C, Env, OutEnv).
block_eval(t_block(C, B), Env, OutEnv) :- command_eval(C, Env, Env1), command_eval(B, Env1, OutEnv).

command_eval(D, Env, OutEnv) :- declaration_eval(D, Env, OutEnv).
command_eval(A, Env, OutEnv) :- assign_eval(A, Env, OutEnv).
command_eval(C, Env, OutEnv) :- conditional_eval(C, Env, OutEnv).
command_eval(F, Env, OutEnv) :- for_eval(F, Env, OutEnv).
command_eval(W, Env, OutEnv) :- while_eval(W, Env, OutEnv).
command_eval(T, Env, OutEnv) :- tern_eval(T, Env, OutEnv).
command_eval(P, Env, OutEnv) :- print_eval(P, Env, OutEnv).

declaration_eval(t_dec(T,I), Env, OutEnv) :- type_eval(T, Type), identifier_eval(I, Identifier), update(Type, Identifier, _, Env, OutEnv).
declaration-eval(t_dec(T,A), Env, OutEnv) :- type_eval(T, Type), assign_eval(A, Env, OutEnv, Type).

type_eval(t_type('int'), int).
type_eval(t_type('String'), string).
type_eval(t_type('boolean'), boolean).

assign_eval(t_assign(I,E), Env, OutEnv, Type) :- identifier_eval(I, Identifier), expression_eval(E, Env, OutEnv, Value), update(Type, Identifier, Value, Env, OutEnv).
assign_eval(t_assign(I,E), Env, OutEnv) :- identifier_eval(I, Identifier), expression_eval(E, Value), lookup(Type, Identifier, _, Env), update(Type, Identifier, Value, Env, OutEnv).

expression_eval(I, Env, _, Result) :- identifier_eval(I, Temp), lookup(_,Temp,Result,Env).
expression_eval(I, _, _, Result) :- integer_eval(I, Result).
expression_eval(S, _, _, Result) :- string_eval(S, Result).
expression_eval(B, Env, OutEnv, Result) :- bool_eval(B, Env, OutEnv, Result).
expression_eval(t_plus(A,B), Env, OutEnv, Result) :- expression_eval(A, Env, OutEnv, Temp1), term_eval(B, Env, OutEnv, Temp2), Result is Temp1 + Temp2.
expression_eval(t_minus(A,B), Env, OutEnv, Result) :- expression_eval(A, Env, OutEnv, Temp1), term_eval(B, Env, OutEnv, Temp2) Result is Temp1 - Temp2.
expression_eval(A, Env, OutEnv, Result) :- term_eval(A, Env, OutEnv, Result).

term_eval(t_times(A, B), Env, OutEnv, Result) :- term_eval(A, Env, OutEnv, Temp1), paren_eval(B, Env, OutEnv, Temp2), Result is Temp1 * Temp2.
term_eval(t_divide(A, B), Env, OutEnv, Result) :- term_eval(A, Env, OutEnv, Temp1), paren_eval(B, Env, OutEnv, Temp2), Result is Temp1 / Temp2.
term_eval(A, Env, OutEnv, Result) :- paren_eval(A, Env, OutEnv, Result).

paren_eval(t_paren(A), Env, OutEnv, Result) :- expression_eval(A, Env, OutEnv, Result).
paren_eval(A, Env, OutEnv, Result) :- value_eval(A, Env, OutEnv, Result).

value_eval(A, Env, _, Result) :- identifier_eval(A, Temp), lookup(_,Temp,Result,Env).
value_eval(A, _, _, Result) :- integer_eval(A, Result).

integer_eval(I, Result) :- {is_of_type(integer, I)}, Result is I.

%Maybe do this as just check if it is a string then just use as is.
%identifier_eval(L, Result) :- letter_eval(L, Result).
%Might want to change how we do our identifiers., Might be able to drop all letters as we only need to deal with strings
%letter_eval(A, Env, OutEnv) :-

bool_eval(t_bool(true), _, _, true).
bool_eval(t_bool(false), _, _, false).
bool_eval(t_bool(E1, '==', E2), Env, OutEnv, true) :- expression_eval(E1, Env, OutEnv, Temp1), expression_eval(E2, Env, OutEnv, Temp2), Temp1 =:= Temp2.
bool_eval(t_bool(E1, '==', E2), Env, OutEnv, false) :- expression_eval(E1, Env, OutEnv, Temp1), expression_eval(E2, Env, OutEnv, Temp2), Temp1 =/= Temp2.
bool_eval(t_bool(not, B), Env, OutEnv, Result):- bool_eval(B, Env, OutEnv, Temp), bool_not(Temp, Result).
bool_eval(t_bool(B1, and, B2), Env, OutEnv, Result) :- bool_eval(B1, Env, OutEnv, Temp1), bool_eval(B2, Env, OutEnv, Temp2), bool_check(Temp1, and, Temp2, Result).
bool_eval(t_bool(B1, or, B2), Env, OutEnv, Result) :- bool_eval(B1, Env, OutEnv, Temp1), bool_eval(B2, Env, OutEnv, Temp2), bool_check(Temp1, or, Temp2, Result).
bool_eval(t_ident(I1, '<', I2), Env, OutEnv, true) :- identifier_eval(I1, Temp1), identifier_eval(I2, Temp2), lookup(_,Temp1,Val1,Env), lookup(_,Temp2,Val2,Env), Val1 < Val2.
bool_eval(t_ident(I1, '<', I2), Env, OutEnv, false) :- identifier_eval(I1, Temp1), identifier_eval(I2, Temp2), lookup(_,Temp1,Val1,Env), lookup(_,Temp2,Val2,Env), Val1 >= Val2.
bool_eval(t_ident(I1, '<=', I2), Env, OutEnv, true) :- identifier_eval(I1, Temp1), identifier_eval(I2, Temp2), lookup(_,Temp1,Val1,Env), lookup(_,Temp2,Val2,Env), Val1 <= Val2.
bool_eval(t_ident(I1, '<=', I2), Env, OutEnv, false) :- identifier_eval(I1, Temp1), identifier_eval(I2, Temp2), lookup(_,Temp1,Val1,Env), lookup(_,Temp2,Val2,Env), Val1 > Val2.
bool_eval(t_ident(I1, '>', I2), Env, OutEnv, true) :- identifier_eval(I1, Temp1), identifier_eval(I2, Temp2), lookup(_,Temp1,Val1,Env), lookup(_,Temp2,Val2,Env), Val1 > Val2.
bool_eval(t_ident(I1, '>', I2), Env, OutEnv, false) :- identifier_eval(I1, Temp1), identifier_eval(I2, Temp2), lookup(_,Temp1,Val1,Env), lookup(_,Temp2,Val2,Env), Val1 <= Val2.
bool_eval(t_ident(I1, '>=', I2), Env, OutEnv, true) :- identifier_eval(I1, Temp1), identifier_eval(I2, Temp2), lookup(_,Temp1,Val1,Env), lookup(_,Temp2,Val2,Env), Val1 >= Val2.
bool_eval(t_ident(I1, '>=', I2), Env, OutEnv, false) :- identifier_eval(I1, Temp1), identifier_eval(I2, Temp2), lookup(_,Temp1,Val1,Env), lookup(_,Temp2,Val2,Env), Val1 < Val2.

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

%String Should be String --> "identifier", not a whole assign statement. Need to fix grammars
string_eval(t_str('"', S, '"'), Result) :- identifier_eval(S, Result).

conditional_eval(t_if(B1,B2), Env, OutEnv) :- bool_eval(B1, Env, OutEnv true), block_eval(B2, Env, OutEnv).
conditional_eval(t_ife(B1,B2,_) Env, OutEnv) :- bool_eval(B1, Env, OutEnv, true), block_eval(B2, Env, OutEnv).
conditional_eval(t_ife(B1,_,B3) Env, OutEnv) :- bool_eval(B1, Env, OutEnv, false), block_eval(B3, Env, OutEnv).

%Requires somekind of result but current language does not allow for this in our structure. Need to change tern
tern_eval(t_tern(B,E1,E2), Env, OutEnv, Result) :- bool_eval(B, Env, OutEnv, true), expression_eval(E1, Env, OutEnv, Result).
tern_eval(t_tern(B,E1,E2), Env, OutEnv, Result) :- bool_eval(B, Env, OutEnv, false), expression_eval(E2, Env, OutEnv, Result).

%I don't know how we should go about for loops
%    for-loop, where boolean expression evaluates to 'true'.
for_eval(t_forstd(A,B1,L,B2), Env, OutEnv) :-
    assign_eval(A, Env, Env1),
    bool_eval(B1, Env1, Env2, true),
    block_eval(B2, Env2, Env3),
    loop_eval(L, Env3, Env4),
    for_helper_eval(t_forstd(A,B1,L,B2), Env4, OutEnv).

%    for-loop, where boolean expression evaluates to 'false'.
for_eval(t_forstd(A, B1, _, _), Env, OutEnv) :-
    assign_eval(A, Env, Env1), b
    ool_eval(B1, Env1, OutEnv, false).

%  Helper predicate that does not evaluate the 'assignment' part of for-loop. Here boolean expression evaluates to 'true'.
for_helper_eval(t_forstd(_, B1, L, B2), Env, OutEnv) :-
    bool_eval(B1, Env, Env2, true),
    block_eval(B2, Env2, Env3),
    loop_eval(L, Env3, Env4),
    for_helper_eval(t_forstd(_, B1, L, B2), Env4, OutEnv).

%  Helper predicate that does not evaluate the 'assignment' part of for-loop. Here boolean expression evaluates to 'false'.
for_helper_eval(t_forstd(_, B1, _, _), Env, OutEnv) :-
    bool_eval(B1, Env, OutEnv, false).

%    Short hand for loop.
%    for i in range(2, 5)
for_eval(t_forrng(_, BoundLower, BoundUpper, Block), Env0, EnvFinal) :-
    BoundLower <= BoundUpper,
    block_eval(Block, Env0, Env1),
    BoundLowerIncremented is BoundLower + 1,
    for_eval(t_forrng(_, BoundLowerIncremented, BoundUpper, Block), Env1, EnvFinal).


while_eval(t_while(B1, B2), Env, OutEnv) :- bool_eval(B1, Env, OutEnv, Bool), while_helper(Bool, B2, Env, OutEnv, B1).
while_eval(t_dowhile(B1, B2), Env, OutEnv) :- bool_eval(B2, Env, OutEnv, Bool), dowhile_helper(Bool, B1, Env, OutEnv, B2).
    
while_helper(true, B2, Env, OutEnv, B1) :- block_eval(B2, Env, Env1), while_eval(t_while(B1, B2), Env1, OutEnv).
while_helper(false, _, Env, Env, _).

dowhile_helper(true, B1, Env, OutEnv, B2) :- block_eval(B1, Env, Env1), while_eval(t_dowhile(B2, B1), Env1, OutEnv).
dowhile_helper(false, _, Env, Env, _).

%Our print as in our langauge can only print identifier values, may need to be changed?
print_eval(t_print(I), Env, OutEnv) :- identifier_eval(I, Result), lookup(_, Result, Val, Env), print(Val).

%need increment, decrement, and other for loop apsects, as well as range

%Taken From Assign 3 Semantics, needs editing, needs types I believe
lookup(Type, Identifier, Number, [(Type, Identifier, Number) | _]).
lookup(Type, Identifier, Number, [_ | T]):- lookup(Type, Identifier, Number, T).

update(Type, Identifier, Number, [], [(Type, Identifier, Number)]).
update(Type, Identifier, Number, [(Type, Identifier, _) | T], [(Type, Identifier, Number) | T]).
update(Type, Identifier, Number, [H | T], [H | Env]):- update(Type, Identifier, Number, T, Env).

%setEnv((Identifier, Number), Env, Result):- EnvTemp = [(Identifier, Number)], append(Env, EnvTemp, Result).
%Don't think we need this, as we aren't using set output values like in assign 3
