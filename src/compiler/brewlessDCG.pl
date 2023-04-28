:- use_rendering(svgtree).
:- table expr/3, term/3, bool/3.

test(cmd,['{',int,"a",;,'}']).
test(cmdlst,['{',int,"a",;,int,"b",=,3,;,'}']).
test(assign,['{',int,"a",;,"a",=,3,;,'}']).
test(if,['{',
            int, "a",;,
            if, '(',true,')', '{',"a", =, 5,;,'}',;,
          '}']).
test(if_else,['{',
                 int, "a",;,
                 if, '(',false,')', '{',"a", =, 5,;,'}',
                 else,'{',"a",=,3,;,'}',;,'}']).
test(for_inc,['{', 
                 for, '(', int, "i", =, 0,;, "i", <, 10,;, "i", ++, ')', 
                 '{', print, '(', "i", ')', ;, '}', ;, 
             '}']).
test(for_dec,['{', 
                 for, '(', int, "i", =, 0,;, "i", <, 10,;, "i", --, ')', 
                 '{', print, '(', "i", ')', ;, '}', ;, 
             '}']).
test(for_assign,['{', 
                  for, '(', int, "i", =, 10,;, "i", >, 0,;, "i", =, "i",-,2, ')', 
                  '{', print, '(', "i", ')', ;, '}', ;,
                '}']).
test(for_rng,['{',
                 int, "i", =, 3,;,
                 for, '(', "i", in, 'Range', '(', 2, ',', 7, ')', ')',
                 '{', print, '(', "i", ')', ;, '}', ;,
               '}']).
test(while,['{',
                int, "start", =, 1,;,
                int, "end", =, 100,;,
                while, '(', "start", <=, "end", ')', '{',
                    print, '(', "start", ')',;,
                    "start", =, "start", *, 2,;,
                '}',;,
            '}']).

test(do_while, ['{',
                    int, "count", =, 7,;,
                    do, '{',
                        print, '(', "count", ')',;,
                        "count", =, "count", +, 2,;,
                    '}', while, '(', "count", <, 5, ')',;,
                    print, '(', "count", ')',;,
                '}']).

test(tern, ['{',
                int, "a", =, 2,
                '(', "a", >, 3, ?, print, '(', true, ')', :, print, '(', false, ')',')',;,
            '}']).

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

dec(t_dec_ass(T,A)) --> type(T), assign(A).
dec(t_dec(T,I)) --> type(T), ident(I).

type(t_type_int(int)) --> [int].
type(t_type_string(string)) --> ['String'].
type(t_type_bool(bool)) --> [boolean].

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
bool(t_bool_ident(E)) --> expr(E).
bool(t_bool_comp(E1,E2)) --> expr(E1), ['=='], expr(E2).
bool(t_bool_not(B)) --> [not], bool(B).
bool(t_bool_and(B1,B2)) --> bool(B1), [and], bool(B2).
bool(t_bool_or(B1,B2)) --> bool(B1), [or], bool(B2).
bool(t_bool_lt(I,E)) --> ident(I), [<], expr(E).
bool(t_bool_lte(I,E)) --> ident(I), [<=], expr(E).
bool(t_bool_gt(I,E)) --> ident(I), [>], expr(E).
bool(t_bool_gte(I,E)) --> ident(I), [>=], expr(E).

string(t_str(I1, I2)) --> ["string"], ident(I1), [=], ['"'], ident(I2), ['"'].

cond(t_if(B1,B2)) --> 
    [if], ['('], bool(B1), [')'], 
    ['{'], block(B2), ['}'].
cond(t_ife(B1,B2,B3)) --> 
    [if], ['('], bool(B1), [')'], 
    ['{'], block(B2), ['}'], 
    [else], ['{'], block(B3), ['}'].

tern(t_tern(B,A1,A2)) --> ['('], bool(B), ['?'], assign(A1), [':'], assign(A2), [')'].

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

print(t_print(E)) --> [print], ['('], expr(E), [')'].

range(t_range(I1,I2)) --> ['Range'], ['('], int(I1), [','], int(I2), [')'].