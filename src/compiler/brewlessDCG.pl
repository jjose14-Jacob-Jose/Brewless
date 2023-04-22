:- use_rendering(svgtree).

prog(t_prog(B)) --> ['{'], block(B), ['}'].

block(t_block(C,B)) --> cmd(C), [;], block(B).
block(t_block(C)) --> cmd(C).

cmd(t_cmd(D)) --> dec(D).
cmd(t_cmd(A)) --> assign(A).
cmd(t_cmd(C)) --> cond(C).
cmd(t_cmd(F)) --> for(F).
cmd(t_cmd(W)) --> while(W).
cmd(t_cmd(T)) --> tern(T).
cmd(t_cmd(P)) --> print(P).

dec(t_dec(T,I)) --> type(T), ident(I).
dec(t_dec(T,I,E)) --> type(T), ident(I), [=], expr(E).

type(t_type(int)) --> [int].
type(t_type(string)) --> ["String"].
type(t_type(bool)) --> [bool].

assign(t_assign(I,E)) --> ident(I), [=], expr(E).

expr(t_plus(A, B)) --> expr(A), [+], term(B).
expr(t_minus(A, B)) --> expr(A), [-], term(B).
expr(A) --> term(A).
term(t_times(A, B)) --> term(A), [*], paren(B).
term(t_divide(A, B)) --> term(A), [/], paren(B).
term(A) --> paren(A).
paren(t_paren(A)) --> ['('], expr(A), [')'].
paren(A) --> value(A).
value(A) --> ident(A) | int(A).

expr(t_expr(I)) --> ident(I).
expr(t_expr(I)) --> int(I).
expr(t_expr(S)) --> string(S).
expr(t_expr(B)) --> bool(B).

int(I) --> [I], {is_of_type(integer, I)}.

%ident(A) --> letter(L), identifer(I), A = howtodothis.
ident(L) --> letter(L).

letter(A) --> [A], {is_alpha(A)}.

bool(t_bool(true)) --> [true].
bool(t_bool(false)) --> [false].
bool(t_bool(E1,E2)) --> expr(E1), ['=='], expr(E2).
bool(t_bool(B)) --> [not], bool(B).
bool(t_bool(B1,B2)) --> bool(B1), [and], bool(B2).
bool(t_bool(B1,B2)) --> bool(B1), [or], bool(B2).

string(t_str(I1, I2)) = [string], ident(I1), [=], ['"'], ident(I2), ['"'].




    















