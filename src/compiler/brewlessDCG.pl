:- use_rendering(svgtree).
:- table expr/3, term/3, bool/3.

program(t_prog(B)) --> ['{'], block(B), ['}'].

block(t_block(C,B)) --> cmd(C), [;], block(B).
block(C) --> cmd(C), [;].

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

type(t_type_int(int)) --> [int].
type(t_type_string(string)) --> ['String'].
type(t_type_bool(bool)) --> [bool].

assign(t_assign(I,E)) --> ident(I), [=], expr(E).

expr(t_plus(A, B)) --> expr(A), [+], term(B).
expr(t_minus(A, B)) --> expr(A), [-], term(B).
expr(A) --> term(A).
term(t_times(A, B)) --> term(A), [*], paren(B).
term(t_divide(A, B)) --> term(A), [/], paren(B).
term(A) --> paren(A).
paren(t_paren(A)) --> ['('], expr(A), [')'].
paren(A) --> value(A).
value(A) --> ident(A) | int(A) | string(A) | bool(A).

int(I) --> [I], {is_of_type(integer, I)}.

ident(I) --> [I], {is_of_type(string, I)}.

bool(t_bool_true(true)) --> [true].
bool(t_bool_false(false)) --> [false].
bool(t_bool_comp(E1,E2)) --> expr(E1), ['=='], expr(E2).
bool(t_bool_not(B)) --> [not], bool(B).
bool(t_bool_and(B1,B2)) --> bool(B1), [and], bool(B2).
bool(t_bool_or(B1,B2)) --> bool(B1), [or], bool(B2).
bool(t_bool_lt(I1,I2)) --> expr(I1), [<], expr(I2).
bool(t_bool_gt(I1,I2)) --> expr(I1), [>], expr(I2).

string(t_str(I1, I2)) --> ["string"], ident(I1), [=], ['"'], ident(I2), ['"'].

cond(t_if(B1,B2)) --> 
    [if], ['('], bool(B1), [')'], 
    ['{'], block(B2), ['}'].
cond(t_ife(B1,B2,B3)) --> 
    [if], ['('], bool(B1), [')'], 
    ['{'], block(B2), ['}'], 
    [else], ['{'], block(B3), ['}'].

tern(t_tern(B,E1,E2)) --> bool(B), ['?'], expr(E1), [':'], expr(E2).

for(t_forstd(A,B1,L,B2)) --> 
    [for], ['('], dec(A), [;], bool(B1), [;], loop(L), [')'],
    	['{'], block(B2), ['}'].

for(t_forrng(I,R,B)) --> 
    [for], ['('], ident(I), [in], range(R), [')'],
    	['{'], block(B), ['}'].                  

while(t_while(B1,B2)) --> [while], ['('], bool(B1), [')'], ['{'], block(B2), ['}'].
while(t_dowhile(B1,B2)) --> [do], ['{'], block(B1), ['}'], [while], ['('], bool(B2), [')'].
    
loop(t_loop(A)) --> assign(A).
loop(t_inc(I)) --> incr(I).
loop(t_dec(D)) --> decr(D).

incr(I) --> ident(I), ['++'].
incr(I) --> ['++'], ident(I).

decr(I) --> ident(I), ['--'].
decr(I) --> ['--'], ident(I).

print(t_print(I)) --> [print], ['('], ident(I), [')'].

range(t_range(I1,I2)) --> ['Range'], ['('], int(I1), [','], int(I2), [')'].