:- use_rendering(svgtree).
:- table expr/3, term/3, bool/3.

test(1,['{',int,"a",;,'}']).
test(2,['{',int,"a",;,int,"b",=,3,;,'}']).
test(3,['{',int,"a",;,"a",=,3,;,'}']).
test(4,['{',int, "a",;,if, '(',true,')', '{',"a", =, 5,;,'}',;,'}']).
test(5,['{',int, "a",;,if, '(',false,')', '{',"a", =, 5,;,'}',else,'{',"a",=,3,;,'}',;,'}']).
test(6,['{',for,'(',int,"i",=,0,;,"i",<,10,;,"i",++,')','{',print,'(',"i",')',;,'}',;,'}']).
test(7,['{',for,'(',int,"i",=,10,;,"i",>,0,;,"i",--,')','{',int,"a",=,"i",;,'}',;,'}']).
test(8,['{',for,'(',int,"i",=,0,;,"i",<,10,;,"i",=,"i",+,2,')','{',int,"a",=,"i",;,'}',;,'}']).
test(9,['{',for, '(',"i", in, 'Range','(',3,',',7,')',')', '{',int, "a", =, "i",;,'}',;,'}']).
test(10,['{',int, "a", =, 3,;,while,'(',"a", '<', 5,')', '{',"a", =, "a", +, 1,;,'}',;,'}']).
test(11,['{',int, "a", =, 3,;,do, '{',"a", =, "a", +, 1,;,'}', while, '(', "a", '>=', 5, ')', ;,'}']).
test(12,['{',int, "a", =, 3,;,int, "b", =, 2,;,"a", ==, "b", ?, "a", =, "a", +, "b", :, "a", =, "b",;,'}']).
test(13,['{','String',"apple",=,'"',"apple",'"',;,'}']).
test(14,['{',boolean,"a",=,true,or,false,;,'}']).
test(15,['{',boolean,"a",=,true,and,false,;,'}']).

program(t_prog(B)) --> ['{'], block(B), ['}'].

block(t_block(C,B)) --> cmd(C), [;], block(B).
block(C) --> cmd(C), [;].

cmd(X) --> dec(X) | assign(X) | cond(X) | for(X) | while(X) | tern(X) | print(X).

dec(t_dec(T,I)) --> type(T), ident(I).
dec(t_dec_ass(T,A)) --> type(T), assign(A).

type(t_type_int(int)) --> [int].
type(t_type_string(string)) --> ['String'].
type(t_type_bool(bool)) --> [bool].

assign(t_assign(I,E)) --> ident(I), [=], expr(E).

expr(I) --> ident(I).
expr(I) --> int(I).
expr(S) --> string(S).
expr(B) --> bool(B).
expr(t_plus(A, B)) --> expr(A), [+], term(B).
expr(t_minus(A, B)) --> expr(A), [-], term(B).
expr(A) --> term(A).
term(t_times(A, B)) --> term(A), [*], paren(B).
term(t_divide(A, B)) --> term(A), [/], paren(B).
term(A) --> paren(A).
paren(t_paren(A)) --> ['('], expr(A), [')'].
paren(A) --> value(A).
value(A) --> ident(A) | int(A).

int(I) --> [I], {is_of_type(integer, I)}.

ident(I) --> [I], {is_of_type(string, I)}.
/*
letter(A) -->
    ['a'], {A='a'} | ['A'], {A='A'} | ['b'], {A='b'} | ['B'], {A='B'} |
    ['c'], {A='c'} | ['C'], {A='C'} | ['d'], {A='d'} | ['D'], {A='D'} | 
    ['e'], {A='e'} | ['E'], {A='E'} | ['f'], {A='f'} | ['F'], {A='F'} | 
    ['g'], {A='g'} | ['G'], {A='G'} | ['h'], {A='h'} | ['H'], {A='H'} |
    ['i'], {A='i'} | ['I'], {A='I'} | ['j'], {A='j'} | ['J'], {A='J'} | 
    ['k'], {A='k'} | ['K'], {A='K'} | ['l'], {A='l'} | ['L'], {A='L'} | 
    ['m'], {A='m'} | ['M'], {A='M'} | ['n'], {A='n'} | ['N'], {A='N'} |
    ['o'], {A='o'} | ['O'], {A='O'} | ['p'], {A='p'} | ['P'], {A='P'} |
    ['q'], {A='q'} | ['Q'], {A='Q'} | ['r'], {A='r'} | ['R'], {A='R'} |
    ['s'], {A='s'} | ['S'], {A='S'} | ['t'], {A='t'} | ['T'], {A='T'} |
    ['u'], {A='u'} | ['U'], {A='U'} | ['v'], {A='v'} | ['V'], {A='V'} |
    ['w'], {A='w'} | ['W'], {A='W'} | ['x'], {A='x'} | ['X'], {A='X'} | 
    ['y'], {A='y'} | ['Y'], {A='Y'} | ['z'], {A='z'} | ['Z'], {A='Z'}.
*/
bool(t_bool_true(true)) --> [true].
bool(t_bool_false(false)) --> [false].
bool(t_bool_comp(E1,E2)) --> expr(E1), ['=='], expr(E2).
bool(t_bool_not(B)) --> [not], bool(B).
bool(t_bool_and(B1,B2)) --> bool(B1), [and], bool(B2).
bool(t_bool_or(B1,B2)) --> bool(B1), [or], bool(B2).
bool(t_bool_lt(I1,I2)) --> ident(I1), [<], int(I2).
bool(t_bool_lteq(I1,I2)) --> ident(I1), [<=], int(I2).
bool(t_bool_gt(I1,I2)) --> ident(I1), [>], int(I2).
bool(t_bool_gteq(I1,I2)) --> ident(I1), [>=], int(I2).

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
    [for], ['('], assign(A), [;], bool(B1), [;], loop(L), [')'],
    	['{'], block(B2), ['}'].
/*
for(t_forrng(I,R,B)) --> 
    [for], ['('], ident(I), [in], range(R), [')'] ['{'], block(B), ['}'].                  
*/
while(t_while(B1,B2)) --> [while], ['('], bool(B1), [')'], ['{'], block(B2), ['}'].
while(t_dowhile(B1,B2)) --> [do], ['{'], block(B1), ['}'], [while], ['('], bool(B2), [')'].
    
loop(t_loop(A)) --> assign(A).
loop(t_incr(I)) --> incr(I).
loop(t_decr(D)) --> decr(D).

incr(I) --> ident(I), ['++'].
incr(I) --> ['++'], ident(I).

decr(I) --> ident(I), ['--'].
decr(I) --> ['--'], ident(I).

print(t_print(I)) --> [print], ['('], ident(I), [')'].

range(t_range(I1,I2)) --> ['Range'], ['('], int(I1), [','], int(I2), [')'].