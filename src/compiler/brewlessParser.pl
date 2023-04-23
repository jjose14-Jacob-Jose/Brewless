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
