:- use_rendering(svgtree).

program(t_prog(B)) --> ['{'], block(B), ['}'].

block(t_block(C,B)) --> command(C), [;], block(B).
block(t_block(C)) --> command(C).

command(t_cmd(D)) --> declaration(D).

declaration(t_dec(T,I)) --> type(T), identifier(I).
declaration(t_dec(T,I,E)) --> type(T), identifier(I), [=], expression(E).


assignment().

expression().
multiplication().
division().
addition().
subtraction().

type(t_type(int)) --> [int].
type(t_type(string)) --> ["String"].
type(t_type(boolean)) --> [boolean].

integer().
boolean().
string().

digit().

% identifier(L) --> letter(L), identifier(_).
identifier(L) --> letter(L).

% FIXME: identifiers
letter(a) --> [x].
letter(b) --> [y].
letter(c) --> [z].

conditional().
ternary().
for_loop().
while_loop().
loop_update().
increment().
decrement().
print().

stub(_, _).