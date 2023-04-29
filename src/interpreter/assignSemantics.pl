%:- use_rendering(svgtree).

% Parse Tree Generation

    :- discontiguous expression/3.
    :- discontiguous term/3.
    :- discontiguous factor/3.

    :- table expression/3.


% 'Program' starting.
    program(t_program(A)) --> k_block(A), ['.'].

% 'Block' starting.
    k_block(t_block(B, D)) --> ['begin'], declaration(B), [';'], command(D), ['end'].


% 'Declaration' starting.
    declaration(t_declaration(A, B, C))--> ['const'], identifier(A), ['='], number(B), [';'], declaration(C).
    declaration(t_declaration(A, B))--> ['var'], identifier(A), [';'], declaration(B).
    declaration(t_declaration(A, B))--> ['const'], identifier(A), ['='], number(B).
    declaration(t_declaration(A))--> ['var'], identifier(A).


% 'Command' starting.
%    command(t_command_identifier_assignment(A, B)) --> identifier(A), [:=], expression(B).
    command(A) --> command_identifier(A).
    command(t_command_if(A, B, C))--> ['if'], boolean_expression(A), ['then'], command(B), ['else'], command(C), ['endif'].
    command(t_command_while(B, D)) --> ['while'], boolean_expression(B), ['do'], command(D), ['endwhile'].
    command(t_command_block(A)) --> k_block(A).

%    command(t_command_identifier_assignment(A, B, C)) --> identifier(A), [:=], expression(B), [';'], command(C).
    command(t_command_composite(A, C)) --> command_identifier(A), [';'], command(C).
    command(t_command_if(A, B, C, D))--> ['if'], boolean_expression(A), ['then'], command(B), ['else'], command(C), ['endif'], [';'], command(D).
    command(t_command_while(B, D, E)) --> ['while'], boolean_expression(B), ['do'], command(D), ['endwhile'], [';'], command(E).
    command(t_command_block(A, B)) --> k_block(A), [';'], command(B).

    command_identifier(t_command_identifier_assignment(A, B)) --> identifier(A), [:=], expression(B).


% 'Boolean Expression' starting.
    boolean_expression(t_boolean_true(true)) --> [true].
    boolean_expression(t_boolean_false(false)) --> [false].
    boolean_expression(t_boolean_not(B))--> ['not'], boolean_expression(B).
    boolean_expression(t_boolean_equals(A, C))--> expression(A), ['='], expression(C).

% 'Expression' starting.
    expression(A) --> term(A).
    term(A) --> factor(A).

    factor(t_number(A)) --> number(A).
    factor(t_identifier(A)) --> identifier(A).
    factor(A) --> ['('], expression(A), [')'].

    expression(t_expression_identifier_assignment(A, C)) --> identifier(A), [':='], expression(C).

	expression(t_expression_addition(A, C)) --> term(A), ['+'], expression(C).
    expression(t_expression_subtraction(A, C)) --> term(A), ['-'], expression(C).

    term(t_expression_multiplication(A, C)) --> factor(A), ['*'], term(C).
    term(t_expression_division(A, C)) --> factor(A), ['/'], term(C).

% 'Identifier' starting
    identifier(x) --> [x].
    identifier(y) --> [y].
    identifier(z) --> [z].
    identifier(u) --> [u].
    identifier(v) --> [v].


% 'Number' starting.
    number(0) --> [0].
    number(1) --> [1].
    number(2) --> [2].
    number(3) --> [3].
    number(4) --> [4].
    number(5) --> [5].
    number(6) --> [6].
    number(7) --> [7].
    number(8) --> [8].
    number(9) --> [9].

% Checks if a tuple is present in a list of tuples.
    lookup(Variable, [(Variable, Value)|_], Value).
    lookup(Variable, [H|T], Value) :-
        (Variable, _) \= H,
        lookup(Variable, T, Value).
    lookup(_, [], _) :-
        write("Variable not found").

% Updates value of a tuple in a list of tuples.
    update(Variable, NewValue, [], [(Variable, NewValue)]).
    update(Variable, NewValue, [(Variable, _)|T], [(Variable, NewValue)|T]).
    update(Variable, NewValue, [H|T], [H|NewT]) :-
        (Variable,_) \= H,
        update(Variable, NewValue, T, NewT).

%    Following methods validates the new value that is to be updated.
    update_verify_and_update(Variable, NewValue, EnvOld, EnvNew) :-
        number(NewValue),
        update(Variable, NewValue, EnvOld, EnvNew).


%    Main (wrapper )function.
    program_eval(P, X, Y, Z) :-
        %format("~n program_eval(~w, ~w, ~w, ~w)", [P, X, Y, Z]),
        Env = [(x, X), (y, Y)],
        %format("~n eval_prog(~w, ~w, ~w)~n", [P, Env, NewEnv]),
        eval_prog(P, Env, NewEnv),
        %format("~n lookup(~w, ~w, ~w)~n", [z, NewEnv, Z]),
        lookup(z, NewEnv, Z).

%   Evaluating Program. P ::= K.
    eval_prog(t_program(X), Env, NewEnv) :-
        %format("~n eval_block(~w, ~w, ~w)~n", [X, Env, NewEnv]),
        eval_block(X, Env, NewEnv).


%    Evaluating a Block. K ::= begin D; C end.
    eval_block(t_block(X, Y), Env, NewEnv) :-
        %format("~n eval_decl(~w, ~w, ~w)~n", [X, Env, EnvAfterX]),
        eval_decl(X, Env, EnvAfterX),
        %format("~n eval_comm(~w, ~w, ~w)~n", [Y, EnvAfterX, NewEnv]),
        eval_comm(Y, EnvAfterX, NewEnv).


%    Evaluating a Declaration. D ::= D;D.
    eval_decl(t_declaration(X, Y), Env, NewEnv) :-
        %format("~n eval_decl(~w, ~w, ~w)~n", [X, Env, EnvAfterX]),
        eval_decl(X, Env, EnvAfterX),
        %format("~n eval_decl(~w, ~w, ~w)~n", [Y, EnvAfterX, NewEnv]),
        eval_decl(Y, EnvAfterX, NewEnv).

%    Evaluating a Declaration. D ::= const I = N.
    eval_decl(t_declaration(X, Y), Env, NewEnv) :-
        %format("~n update(~w, ~w, ~w, ~w)~n", [X, Y, Env, NewEnv]),
        update_verify_and_update(X, Y, Env, NewEnv).

%    Evaluating a Declaration. D ::= var I.
    eval_decl(t_declaration(X), Env, NewEnv) :-
        %format("~n update(~w, ~w, ~w, ~w)~n", [X, Y, Env, NewEnv]),
        eval_decl(X, Env, NewEnv).

%    Evaluating a Declaration. D ::= var I.
    eval_decl(X, Env, NewEnv) :-
        X \=x,
        X \=y,
        %format("~n update(~w, ~w, ~w, ~w)~n", [X, 0, Env, NewEnv]),
        update_verify_and_update(X, 0, Env, NewEnv).

    eval_decl(X, Env, Env) :-
        X = x.

    eval_decl(X, Env, Env) :-
        X = y.


%    Evaluating command. C :: = C;C.
    eval_comm(t_command_composite(X, Y), Env, NewEnv) :-
        eval_comm(X, Env, EnvAfterX),
        eval_comm(Y, EnvAfterX, NewEnv).

%    Evaluating command. C :: = I := E.
    eval_comm(t_command_identifier_assignment(X, Y), Env, NewEnv) :-
        eval_expr(Y, Env, ValueAfterY, EnvAfterY),
        update_verify_and_update(X, ValueAfterY, EnvAfterY, NewEnv).

%    Evaluating command. C :: = if B then J else K end if. ('B' is 'true').
    eval_comm(t_command_if(X, J, _), Env, NewEnv) :-
        eval_bool(X, Env, ValueAfterX, EnvAfterX),
        ValueAfterX = true,
        eval_comm(J, EnvAfterX, NewEnv).

%    Evaluating command. C :: = if B then J else K end if. ('B' is 'false').
    eval_comm(t_command_if(X, _, K), Env, NewEnv) :-
        eval_bool(X, Env, ValueAfterX, EnvAfterX),
        ValueAfterX = false,
        eval_comm(K, EnvAfterX, NewEnv).

%    Evaluating command. C :: = while B do C endwhile. ('B' is 'true').
    eval_comm(t_command_while(B, C), Env, NewEnv) :-
        eval_bool(B, Env, ValueAfterB, EnvAfterB),
        ValueAfterB = true,
        eval_comm(C, EnvAfterB, EnvAfterC),
        eval_comm(t_command_while(B, C), EnvAfterC, NewEnv).

%    Evaluating command. C :: = while B do C endwhile. ('B' is 'false').
    eval_comm(t_command_while(B, _), Env, NewEnv) :-
        eval_bool(B, Env, ValueAfterB, NewEnv),
        ValueAfterB = false.


%    Evaluating command. C :: = K.
    eval_comm(t_command_block(X), Env, NewEnv) :-
        eval_block(X, Env, NewEnv).


%    Evaluating Boolean expression. B ::= true.
    eval_bool(t_boolean_true(true), _, true, _).

%    Evaluating Boolean expression. B ::= false.
    eval_bool(t_boolean_false(false), _, false, _).

%    Evaluating Boolean expression. B ::= E = E. LHS == RHS.
    eval_bool(t_boolean_equals(X, Y), Env, FlagIsEqual, NewEnv) :-
        eval_expr(X, Env, ValueAfterX, EnvAfterX),
        eval_expr(Y, EnvAfterX, ValueAfterY, NewEnv),
        ValueAfterX = ValueAfterY,
        eval_true(FlagIsEqual).


%    Evaluating Boolean expression. B ::= E = E. LHS != RHS.
    eval_bool(t_boolean_equals(X, Y), Env, FlagIsEqual, NewEnv) :-
        eval_expr(X, Env, ValueAfterX, EnvAfterX),
        eval_expr(Y, EnvAfterX, ValueAfterY, NewEnv),
        ValueAfterX \= ValueAfterY,
        eval_false(FlagIsEqual).

%    Evaluating Boolean expression. B ::= not B. 'B' on RHS is true.
    eval_bool(t_boolean_not(X), Env, false, NewEnv) :-
        eval_bool(X, Env, ValueAfterX, NewEnv),
        ValueAfterX = true.

%    Evaluating Boolean expression. B ::= not B. 'B' on RHS is false.
    eval_bool(t_boolean_not(X), Env, true, NewEnv) :-
        eval_bool(X, Env, ValueAfterX, NewEnv),
        ValueAfterX = false.


%    Evaluating expression. E ::= E + E.
    eval_expr(t_expression_addition(X, Y), Env, Value, NewEnv) :-
        eval_expr(X, Env, ValueAfterX, EnvAfterX),
        eval_expr(Y, EnvAfterX, ValueAfterY, NewEnv),
        Value is ValueAfterX + ValueAfterY.

%    Evaluating expression. E ::= E - E.
    eval_expr(t_expression_subtraction(X, Y), Env, Value, NewEnv) :-
        eval_expr(X, Env, ValueAfterX, EnvAfterX),
        eval_expr(Y, EnvAfterX, ValueAfterY, NewEnv),
        Value is ValueAfterX - ValueAfterY.

%    Evaluating expression. E ::= E * E.
    eval_expr(t_expression_multiplication(X, Y), Env, Value, NewEnv) :-
        eval_expr(X, Env, ValueAfterX, EnvAfterX),
        eval_expr(Y, EnvAfterX, ValueAfterY, NewEnv),
        Value is ValueAfterX * ValueAfterY.

%    Evaluating expression. E ::= E / E.
    eval_expr(t_expression_division(X, Y), Env, Value, NewEnv) :-
        eval_expr(X, Env, ValueAfterX, EnvAfterX),
        eval_expr(Y, EnvAfterX, ValueAfterY, NewEnv),
        Value is ValueAfterX / ValueAfterY.

%    Evaluating expression. E ::= (E).
    eval_expr(t_expression(X), Env, Value, NewEnv) :-
        eval_expr(X, Env, Value, NewEnv).

%    Evaluating expression. E ::= I = E.
    eval_expr(t_expression_identifier_assignment(X, Y), Env, Value, NewEnv) :-
        eval_expr(Y, Env, Value, EnvAfterY),
        update_verify_and_update(X, Value, EnvAfterY, NewEnv).

%    Evaluating expression. E ::= I.
    eval_expr(t_identifier(X), Env, Value, Env) :-
        lookup(X, Env, Value).

%    Evaluating expression. E ::= N.
    eval_expr(t_number(X), Env, X, Env).

    eval_true(true).
    eval_false(false).

% Main class

execute_program(ListOfTokens, Z) :-
    program(Tree, ListOfTokens, []),
    program_eval(Tree, 2, 3, Z).

































