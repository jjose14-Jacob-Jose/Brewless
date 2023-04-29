:- table expr/3, term/3, bool/3.

program(t_prog(B)) --> ['{'], block(B), ['}'].

block(t_block(C,B)) --> cmd(C), [';'], block(B).
block(C) --> cmd(C), [';'].

cmd(X) --> 
    dec(X) | 
    assign(X) | 
    cond(X) | 
    for(X) | 
    while(X) | 
    tern(X) | 
    print(X).

dec(t_dec(T,I)) --> type(T), ident(I).
dec(t_dec_ass(T,A)) --> type(T), assign(A).

type(t_type_int(int)) --> ['int'].
type(t_type_string(string)) --> ['String'].
type(t_type_bool(bool)) --> ['bool'].

assign(t_assign(I,E)) --> ident(I), ['='], expr(E).

expr(t_plus(A, B)) --> expr(A), ['+'], term(B).
expr(t_minus(A, B)) --> expr(A), ['-'], term(B).
expr(A) --> term(A).
term(t_times(A, B)) --> term(A), ['*'], paren(B).
term(t_divide(A, B)) --> term(A), ['/'], paren(B).
term(A) --> paren(A).
paren(t_paren(A)) --> ['('], expr(A), [')'].
paren(A) --> value(A).
value(A) --> ident(A) | int(A) | str(A) | bool(A).

int(I) --> [I], {is_of_type(integer, I)}.

ident(I) --> [I], {atom(I)}.

bool(t_bool_true(true)) --> ['true'].
bool(t_bool_false(false)) --> ['false'].
bool(t_bool_comp(E1,E2)) --> expr(E1), ['=='], expr(E2).
bool(t_bool_not(B)) --> ['not'], bool(B).
bool(t_bool_and(B1,B2)) --> bool(B1), ['and'], bool(B2).
bool(t_bool_or(B1,B2)) --> bool(B1), ['or'], bool(B2).
bool(t_bool_lt(I1,I2)) --> expr(I1), ['<'], expr(I2).
bool(t_bool_gt(I1,I2)) --> expr(I1), ['>'], expr(I2).

str(t_str(I1, I2)) --> ['String'], ident(I1), ['='], ident(I2).

cond(t_if(B1,B2)) --> 
    ['if'], ['('], bool(B1), [')'], 
    ['{'], block(B2), ['}'].
cond(t_ife(B1,B2,B3)) --> 
    ['if'], ['('], bool(B1), [')'], 
    ['{'], block(B2), ['}'], 
    ['else'], ['{'], block(B3), ['}'].

tern(t_tern(B,E1,E2)) --> ['('], bool(B), ['?'], expr(E1), [':'], expr(E2), [')'].

for(t_forstd(A,B1,L,B2)) --> 
    ['for'], ['('], dec(A), [';'], bool(B1), [';'], loop(L), [')'],
    	['{'], block(B2), ['}'].

for(t_forrng(I,R,B)) --> 
    ['for'], ['('], ident(I), ['in'], range(R), [')'],
    	['{'], block(B), ['}'].                  

while(t_while(B1,B2)) --> ['while'], ['('], bool(B1), [')'], ['{'], block(B2), ['}'].
while(t_dowhile(B1,B2)) --> ['do'], ['{'], block(B1), ['}'], ['while'], ['('], bool(B2), [')'].
    
loop(t_loop(A)) --> assign(A).
loop(t_inc(I)) --> incr(I).
loop(t_dec(D)) --> decr(D).

incr(I) --> ident(I), ['++'].
incr(I) --> ['++'], ident(I).

decr(I) --> ident(I), ['--'].
decr(I) --> ['--'], ident(I).

print(t_print(I)) --> ['print'], ['('], ident(I), [')'].

range(t_range(I1,I2)) --> ['Range'], ['('], int(I1), [','], int(I2), [')'].

program_eval(t_prog(B), OutEnv) :- block_eval(B, [], OutEnv).

block_eval((C), Env, OutEnv) :- command_eval(C, Env, OutEnv).
block_eval(t_block(C, B), Env, OutEnv) :- command_eval(C, Env, Env1), block_eval(B, Env1, OutEnv).

command_eval(D, Env, OutEnv) :- declaration_eval(D, Env, OutEnv).
command_eval(A, Env, OutEnv) :- assign_eval(A, Env, OutEnv).
command_eval(C, Env, OutEnv) :- conditional_eval(C, Env, OutEnv).
command_eval(F, Env, OutEnv) :- for_eval(F, Env, OutEnv).
command_eval(W, Env, OutEnv) :- while_eval(W, Env, OutEnv).
%command_eval(T, Env, OutEnv) :- tern_eval(T, Env, OutEnv).
command_eval(P, Env, OutEnv) :- print_eval(P, Env, OutEnv).

declaration_eval(t_dec(T,I), Env, OutEnv) :- type_eval(T, Type), identifier_eval(I, Identifier), update(Type, Identifier, _, Env, OutEnv).
declaration_eval(t_dec_ass(T,A), Env, OutEnv) :- type_eval(T, Type), assign_eval(A, Env, OutEnv, Type).

type_eval(t_type_int('int'), int).
type_eval(t_type_string('String'), string).
type_eval(t_type_boolean('boolean'), boolean).

assign_eval(t_assign(I,E), Env, OutEnv, Type) :- identifier_eval(I, Identifier), expression_eval(E, Env, _, Value), update(Type, Identifier, Value, Env, OutEnv).
assign_eval(t_assign(I,E), Env, OutEnv) :- identifier_eval(I, Identifier), expression_eval(E, Env, _, Value), lookup(Type, Identifier, _, Env), update(Type, Identifier, Value, Env, OutEnv).

%expression_eval(I, Env, _, Result) :- identifier_eval(I, Temp), lookup(_,Temp,Result,Env).
%expression_eval(I, _, _, Result) :- integer_eval(I, Result).
%expression_eval(S, _, _, Result) :- string_eval(S, Result).
%expression_eval(B, Env, OutEnv, Result) :- bool_eval(B, Env, OutEnv, Result).
expression_eval(t_plus(A,B), Env, OutEnv, Result) :- expression_eval(A, Env, OutEnv, Temp1), term_eval(B, Env, OutEnv, Temp2), Result is Temp1 + Temp2.
expression_eval(t_minus(A,B), Env, OutEnv, Result) :- expression_eval(A, Env, OutEnv, Temp1), term_eval(B, Env, OutEnv, Temp2), Result is Temp1 - Temp2.
expression_eval(A, Env, OutEnv, Result) :- term_eval(A, Env, OutEnv, Result).

term_eval(t_times(A, B), Env, OutEnv, Result) :- term_eval(A, Env, OutEnv, Temp1), paren_eval(B, Env, OutEnv, Temp2), Result is Temp1 * Temp2.
term_eval(t_divide(A, B), Env, OutEnv, Result) :- term_eval(A, Env, OutEnv, Temp1), paren_eval(B, Env, OutEnv, Temp2), Result is Temp1 / Temp2.
term_eval(A, Env, OutEnv, Result) :- paren_eval(A, Env, OutEnv, Result).

paren_eval(t_paren(A), Env, OutEnv, Result) :- expression_eval(A, Env, OutEnv, Result).
paren_eval(A, Env, OutEnv, Result) :- value_eval(A, Env, OutEnv, Result).

value_eval(A, Env, _, Result) :- identifier_eval(A, Temp), lookup(_,Temp,Result,Env).
value_eval(A, _, _, Result) :- integer_eval(A, Result).
value_eval(B, Env, OutEnv, Result) :- bool_eval(B, Env, OutEnv, Result).

integer_eval(I, Result) :- atom_number(I, Result).

identifier_eval(I, Result) :- atom(I), Result = I.

bool_eval(t_bool_true(true), _, _, true).
bool_eval(t_bool_false(false), _, _, false).
bool_eval(t_bool_comp(E1, E2), Env, OutEnv, true) :- expression_eval(E1, Env, OutEnv, Temp1), expression_eval(E2, Env, OutEnv, Temp2), Temp1 =:= Temp2.
bool_eval(t_bool_comp(E1, E2), Env, OutEnv, false) :- expression_eval(E1, Env, OutEnv, Temp1), expression_eval(E2, Env, OutEnv, Temp2), Temp1 =\= Temp2.
bool_eval(t_bool_not(not, B), Env, OutEnv, Result):- bool_eval(B, Env, OutEnv, Temp), bool_not(Temp, Result).
bool_eval(t_bool_and(B1, and, B2), Env, OutEnv, Result) :- bool_eval(B1, Env, OutEnv, Temp1), bool_eval(B2, Env, OutEnv, Temp2), bool_check(Temp1, and, Temp2, Result).
bool_eval(t_bool_or(B1, or, B2), Env, OutEnv, Result) :- bool_eval(B1, Env, OutEnv, Temp1), bool_eval(B2, Env, OutEnv, Temp2), bool_check(Temp1, or, Temp2, Result).
bool_eval(t_bool_lt(I1, I2), Env, OutEnv, true) :- expression_eval(I1, Env, OutEnv, Temp1),
    expression_eval(I2, Env, OutEnv, Temp2), 
    Temp1 < Temp2.
bool_eval(t_bool_lt(I1, I2), Env, OutEnv, false) :- expression_eval(I1, Env, OutEnv, Temp1),
    expression_eval(I2, Env, OutEnv, Temp2), 
    Temp1 >= Temp2.
%bool_eval(t_ident(I1, '<=', I2), Env, _, true) :- identifier_eval(I1, Temp1), identifier_eval(I2, Temp2), lookup(_,Temp1,Val1,Env), lookup(_,Temp2,Val2,Env), Val1 =< Val2.
%bool_eval(t_ident(I1, '<=', I2), Env, _, false) :- identifier_eval(I1, Temp1), identifier_eval(I2, Temp2), lookup(_,Temp1,Val1,Env), lookup(_,Temp2,Val2,Env), Val1 > Val2.
bool_eval(t_bool_gt(I1, I2), Env, OutEnv, true) :- expression_eval(I1, Env, OutEnv, Temp1), 
    expression_eval(I2, Env, OutEnv, Temp2), 
    Temp1 > Temp2.
bool_eval(t_bool_gt(I1, I2), Env, _, false) :- expression_eval(I1, Env, OutEnv, Temp1), 
    expression_eval(I2, Env, OutEnv, Temp2), 
    Temp1 =< Temp2.
%bool_eval(t_ident(I1, '>=', I2), Env, _, true) :- identifier_eval(I1, Temp1), identifier_eval(I2, Temp2), lookup(_,Temp1,Val1,Env), lookup(_,Temp2,Val2,Env), Val1 >= Val2.
%bool_eval(t_ident(I1, '>=', I2), Env, _, false) :- identifier_eval(I1, Temp1), identifier_eval(I2, Temp2), lookup(_,Temp1,Val1,Env), lookup(_,Temp2,Val2,Env), Val1 < Val2.

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
string_eval(t_str(S), Result) :- identifier_eval(S, Result).

conditional_eval(t_if(B1,B2), Env, OutEnv) :- bool_eval(B1, Env, OutEnv, true), block_eval(B2, Env, OutEnv).
conditional_eval(t_ife(B1,B2,_), Env, OutEnv) :- bool_eval(B1, Env, OutEnv, true), block_eval(B2, Env, OutEnv).
conditional_eval(t_ife(B1,_,B3), Env, OutEnv) :- bool_eval(B1, Env, OutEnv, false), block_eval(B3, Env, OutEnv).

%Requires somekind of result but current language does not allow for this in our structure. Need to change tern
%tern_eval(t_tern(B,E1,_), Env, OutEnv, Result) :- bool_eval(B, Env, OutEnv, true), expression_eval(E1, Env, OutEnv, Result).
%tern_eval(t_tern(B,_,E2), Env, OutEnv, Result) :- bool_eval(B, Env, OutEnv, false), expression_eval(E2, Env, OutEnv, Result).

%I don't know how we should go about for loops
%    for-loop, where boolean expression evaluates to 'true'.
for_eval(t_forstd(A,B1,L,B2), Env, OutEnv) :-
    declaration_eval(A, Env, Env1),
    bool_eval(B1, Env1, _, true),
    loop_eval(L, Env1, Env3),
    block_eval(B2, Env3, Env4),
    for_helper_eval(t_forstd(A,B1,L,B2), Env4, OutEnv).
%    for-loop, where boolean expression evaluates to 'false'.
for_eval(t_forstd(A, B1, _, _), Env, OutEnv) :-
    declaration_eval(A, Env, Env1),
    bool_eval(B1, Env1, OutEnv, false).
%    Short hand for loop.
%    for i in range(2, 5)
for_eval(t_forrng(_, t_range(I1,I2), Block), Env, EnvFinal) :-
    %declaration_eval(t_dec_ass(Id, I1), Env, Env0),
    I2 > I1,
    I3 = I1+1,
    block_eval(Block, Env, Env1),
    for_helper_eval(t_forrng(t_range(I3,I2), Block), Env1, EnvFinal).

for_helper_eval(t_forrng(t_range(I1,I2), Block), Env, OutEnv) :-
    I2 > I1,
    I3 = I1+1,
    block_eval(Block, Env, Env1),
    for_helper_eval(t_forrng(t_range(I3,I2), Block), Env1, OutEnv).

for_helper_eval(t_forrng(t_range(I1,I2), _), Env, Env) :-
    I2 =< I1.

%  Helper predicate that does not evaluate the 'assignment' part of for-loop. Here boolean expression evaluates to 'true'.
for_helper_eval(t_forstd(_, B1, L, B2), Env, OutEnv) :-
    bool_eval(B1, Env, _, true),
    loop_eval(L, Env, Env3),
    block_eval(B2, Env3, Env4),
    for_helper_eval(t_forstd(_, B1, L, B2), Env4, OutEnv).
%  Helper predicate that does not evaluate the 'assignment' part of for-loop. Here boolean expression evaluates to 'false'.
for_helper_eval(t_forstd(_, B1, _, _), Env, OutEnv) :-
    bool_eval(B1, Env, OutEnv, false).

loop_eval(t_loop(A), Env, OutEnv) :- assign_eval(A, Env, OutEnv).
loop_eval(t_inc(I), Env, OutEnv) :- inc_eval(I, Env, OutEnv).
loop_eval(t_dec(D), Env, OutEnv) :- dec_eval(D, Env, OutEnv).

inc_eval(I, Env, OutEnv) :- lookup(_, I, Val, Env), ValNew is Val + 1, update(_, I, ValNew, Env, OutEnv).
dec_eval(I, Env, OutEnv) :- lookup(_, I, Val, Env), ValNew is Val - 1, update(_, I, ValNew, Env, OutEnv).

while_eval(t_while(B1, B2), Env, OutEnv) :- bool_eval(B1, Env, OutEnv, Bool), while_helper(Bool, B2, Env, OutEnv, B1).
while_eval(t_dowhile(B1, B2), Env, OutEnv) :- bool_eval(B2, Env, OutEnv, Bool), dowhile_helper(Bool, B1, Env, OutEnv, B2).
    
while_helper(true, B2, Env, OutEnv, B1) :- block_eval(B2, Env, Env1), while_eval(t_while(B1, B2), Env1, OutEnv).
while_helper(false, _, Env, Env, _).

dowhile_helper(true, B1, Env, OutEnv, B2) :- block_eval(B1, Env, Env1), while_eval(t_dowhile(B2, B1), Env1, OutEnv).
dowhile_helper(false, _, Env, Env, _).

print_eval(t_print(I), Env, OutEnv) :- identifier_eval(I, Result), lookup(_, Result, Val, Env), print(Val), OutEnv = Env.

lookup(Type, Identifier, Number, [(Type, Identifier, Number) | _]).
lookup(Type, Identifier, Number, [_ | T]):- lookup(Type, Identifier, Number, T).

update(Type, Identifier, Number, [], [(Type, Identifier, Number)]).
update(Type, Identifier, Number, [(Type, Identifier, _) | T], [(Type, Identifier, Number) | T]).
update(Type, Identifier, Number, [H | T], [H | Env]):- update(Type, Identifier, Number, T, Env).
