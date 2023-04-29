consult('brewlessDCG.pl').
consult('brewlessParser.pl').

compile_and_run_brewless(ListOfTokens, Out_Env) :-
    program(Parse_Tree, ListOfTokens, []),
    program_eval(Parse_Tree, Out_Env).