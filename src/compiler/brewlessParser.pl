program_eval(t_prog(B)) :- block_eval(B, [], OutEnv).

block_eval(t_block(C), Env, OutEnv) :- command_eval(C, Env, OutEnv).
block_eval(t_block(C, B), Env, OutEnv) :- command_eval(C, Env, Env1), command_eval(B, Env1, OutEnv).

command_eval(t_cmd(D), Env, OutEnv) :- declaration_eval(D, Env, OutEnv).

declaration_eval(t_dec(T,I), Env, OutEnv) :- type_eval(T, Env, Env1), identifier_eval(I, Env1, OutEnv).
declaration-eval(t_dec(T,I,E), Env, OutEnv) :- type_eval(T, Env, Env1), identifier_eval(I, Env1, Env2), expression_eval(E, Env2, OutEnv).
