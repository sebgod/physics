:- module(utils, [
                  call_semidet_ground/2,
                  call_semidet_ground/3,
                  call_semidet_ground_first/3,
                  call_semidet_ground_first/4,
                  safe_is/2,
                  numeric_inverse/2,
                  term_sup/2,
                  term_sub/2,
                  number_sign_sup/2,
                  number_sign_sub/2,
                  capitalize/2,
                  map_term_codes/3,
                  between2d/4,
                  findnsols/4
                 ]).
:- meta_predicate
    call_semidet_ground(1, ?),
    call_semidet_ground(2, ?, ?),
    call_semidet_ground_first(2, ?,?),
    call_semidet_ground_first(3,?,?,?),
    findnsols(+, ?, :, -),
    findnsols(+, ?, :, -, ?),
    maxsols(?, 0),
    map_term_codes(2, +, ?).

:- use_module(library(clpfd)).

%%	call_semidet_ground(:Goal, +Var) is semidet.
%%	call_semidet_ground(:Goal, ?Var) is nondet.
call_semidet_ground(Goal, Var) :-
    (   ground(Var)
    ->  call(Goal, Var), !
    ;   call(Goal, Var)
    ).

%%	call_semidet_ground(:Goal, +V1, +V2) is semidet.
%%	call_semidet_ground(:Goal, ?V1, ?V2) is nondet.
call_semidet_ground(Goal, V1, V2) :-
    (   ground(V1), ground(V2)
    ->  call(Goal, V1, V2), !
    ;   call(Goal, V1, V2)
    ).

%%	call_semidet_ground_first(:Goal, +V1, ?V2) is semidet.
%%	call_semidet_ground_first(:Goal, ?V1, ?V2) is nondet.
call_semidet_ground_first(Goal, V1, V2) :-
    (   ground(V1)
    ->  call(Goal, V1, V2), !
    ;   call(Goal, V1, V2)
    ).

%%	call_semidet_ground_first(:Goal, +V1, ?V2, ?V3) is semidet.
%%	call_semidet_ground_first(:Goal, ?V1, ?V2, ?V3) is nondet.
call_semidet_ground_first(Goal, V1, V2, V3) :-
    (   ground(V1)
    ->  call(Goal, V1, V2, V3), !
    ;   call(Goal, V1, V2, V3)
    ).


%%	safe_is(+A, +B) is semidet.
%%	safe_is(-A, +B) is det.
safe_is(A, B) :-
    (   A == B                -> A = B, _ is A
    ;   ground(B), var(A)     -> A is B
    ;   ground(A), var(B)     -> B is A
    ;   ground(A), ground(B)  -> A =:= B
    ;   numeric_inverse(A, B)
    ).

%%  numeric_inverse(+Number:number,+Number:number) is semidet.
%%  numeric_inverse(?Number:number,?Number:number) is det.
numeric_inverse(A, B)  :- ground(A), ground(B), !, -A =:= B. % green cut
numeric_inverse(-A, B) :- numeric_inverse_(B, -A), !. % green cut
numeric_inverse(A, -B) :- numeric_inverse_(A, -B).

numeric_inverse_(A, -B) :-
    var(B),
    (   ground(A) -> B is -A
    ;   A #= -B
    ).

number_sign_sup(Number, Sup) :-
    Sign is sign(Number),
    (   Sign =:= 0  -> Sup = ''
    ;   Sign =:= +1 -> Sup = '⁺'
    ;   Sign =:= -1 -> Sup = '⁻'
    ).

number_sign_sub(Number, Sub) :-
    Sign is sign(Number),
    (   Sign =:= 0  -> Sub = ''
    ;   Sign =:= +1 -> Sub = '₊'
    ;   Sign =:= -1 -> Sub = '₋'
    ).

term_sup(Term, Symbol) :-
    map_term_codes(term_sup_map, Term, Symbol).

term_sub(Term, Symbol) :-
    map_term_codes(term_sub_map, Term, Symbol).


%%	map_term_codes(:Map, +Term, -Symbol) is semidet.
map_term_codes(Map, Term, Symbol) :-
    term_to_atom(Term, Atom),
    atom_codes(Atom, AtomCodes),
    length(AtomCodes, Length),
    length(SymbolCodes, Length),
    maplist(Map, AtomCodes, SymbolCodes),
    atom_codes(Symbol, SymbolCodes).

term_sub_map(Code, Sub) :-
    (   between(0'0, 0'9, Code) -> Sub is 0x2080 + Code - 0'0
    ;   Code = 0'+  -> Sub = 0'₊
    ;   Code = 0'-  -> Sub = 0'₋
    ;   Code = 0'=  -> Sub = 0x208C
    ;   Code = 0x20 -> Sub = 0x200B
    ;   Code = 0x28 -> Sub = 0x208D
    ;   Code = 0x29 -> Sub = 0x208E
    ;   Code = 0xA0 -> Sub = 0xA0
    ;   Code = 0'a  -> Sub = 0x2090
    ;   Code = 0'b  -> Sub = 0x1D66
    ;   Code = 0'e  -> Sub = 0x2091
    ;   Code = 0'g  -> Sub = 0x1D67
    ;   Code = 0'h  -> Sub = 0x2095
    ;   Code = 0'i  -> Sub = 0x1D62
    ;   Code = 0'k  -> Sub = 0x2096
    ;   Code = 0'l  -> Sub = 0x2097
    ;   Code = 0'm  -> Sub = 0x2098
    ;   Code = 0'n  -> Sub = 0x2099
    ;   Code = 0'o  -> Sub = 0x2092
    ;   Code = 0'p  -> Sub = 0x209A
    ;   Code = 0'P  -> Sub = 0x1D69
    ;   Code = 0'r  -> Sub = 0x1D63
    ;   Code = 0'R  -> Sub = 0x1D68
    ;   Code = 0's  -> Sub = 0x209B
    ;   Code = 0'S  -> Sub = 0x2094
    ;   Code = 0't  -> Sub = 0x209C
    ;   Code = 0'u  -> Sub = 0x1D64
    ;   Code = 0'v  -> Sub = 0x1D65
    ;   Code = 0'x  -> Sub = 0x2093
    ;   Code = 0'X  -> Sub = 0x1D6A
    ).
term_sup_map(Code, Sup) :-
    (   between(0'4, 0'9, Code) -> Sup is 0x2070 + Code - 0'0
    ;   Code = 0'0  -> Sup = 0x2070
    ;   Code = 0'1  -> Sup = 0'¹
    ;   Code = 0'2  -> Sup = 0'²
    ;   Code = 0'3  -> Sup = 0'³
    ;   Code = 0'+  -> Sup = 0'⁺
    ;   Code = 0'-  -> Sup = 0'⁻
    ;   Code = 0'i  -> Sup = 0x2071
    ;   Code = 0'n  -> Sup = 0'ⁿ
    ;   Code = 0'=  -> Sup = 0x207c
    ;   Code = 0x28 -> Sup = 0x207d
    ;   Code = 0x29 -> Sup = 0x207e
    ;   Code = 0x20 -> Sup = 0x200B
    ).

capitalize(Atom, Capitalized) :-
    sub_atom(Atom, 0, 1, RL, First),
    sub_atom(Atom, 1, RL, 0, Rest),
    upcase_atom(First, FirstUp),
    downcase_atom(Rest, RestDown),
    atom_concat(FirstUp, RestDown, Capitalized).

between2d(XR, YR, X, Y) :-
    call_semidet_ground(between2d_nd(XR, YR), X, Y).
between2d_nd(Xmin-Xmax, Ymin-Ymax, X, Y) :-
    (   ground(X) -> number(X) ; true ),
    (   ground(Y) -> number(Y) ; true ),
    between(Xmin, Xmax, X),
    between(Ymin, Ymax, Y).

%%      findnsols(+N, ?Template, :Generator, -List)
%
%       As findall/3, but generating at most   N solutions of Generator.
%       Thus, the length of List will not   be  greater than N. If N=<0,
%       returns directly an empty  list.   This  predicate is especially
%       useful if Generator may have an infinite number of solutions.
%
%       @compat ciao

findnsols(N, Template, Generator, List) :-
        findnsols(N, Template, Generator, List, []).

%%      findnsols(+N, ?Template, :Generator, -List, -Tail)
%
%       As findnsols/4, but returning in Tail the tail of List.
%
%       @compat ciao

findnsols(N, Template, Generator, List, Tail) :-
        findall(Template, maxsols(N, Generator), List, Tail).

maxsols(N, Generator) :-
        State = count(0),
        Generator,
        arg(1, State, C0),
        C1 is C0+1,
        (   C1 == N
        ->  !
        ;   nb_setarg(1, State, C1)
        ).

:- begin_tests(utils).

test('numeric_inverse(1, -1)') :- numeric_inverse(1, -1).
test('number_sign_sub(-1)', Sub == '₋') :- number_sign_sub(-1, Sub).
test('number_sign_sub( 0)', Sub == '' ) :- number_sign_sub( 0, Sub).
test('number_sign_sub(+1)', Sub == '₊') :- number_sign_sub(+1, Sub).
test('number_sign_sup(-1)', Sub == '⁻') :- number_sign_sup(-1, Sub).
test('number_sign_sup( 0)', Sub == '' ) :- number_sign_sup( 0, Sub).
test('number_sign_sup(+1)', Sub == '⁺') :- number_sign_sup(+1, Sub).


test('safe_is(A, -B)', [-A =:= B]) :-
    safe_is(A, -B), B = 4.
test('safe_is(0,1)', [fail]) :- safe_is(0, 1).
test('safe_is(A, 1)', [A =:= 1]) :- safe_is(A, 1).
test('safe_is(pi, pi)') :- safe_is(pi, pi).
test('safe_is(A, pi)', [setup(Pi is pi)]) :- safe_is(Pi, pi).
test('safe_is(A, hello)',
     [throws(error(type_error(evaluable,hello/0),
                   context(system:is/2,_))
            )]
    ) :-
    safe_is(_, hello).
test('safe_is(1 =:= sin(pi/2))') :- safe_is(1, sin(pi/2)).
test('safe_is(-A, -3)', [A =:= 3]) :- safe_is(-A, -3).
test('safe_is(-3, -A)', [A =:= 3]) :- safe_is(-3, -A).
test('safe_is(-pi, A)', [A =:= -pi]) :- safe_is(-pi, A).
test('safe_is(A, -pi)', [A =:= -pi]) :- safe_is(A, -pi).
test('safe_is(-A, -pi)', [A =:= pi]) :- safe_is(-A, -pi).

test('term_sup(23)', Sup == '²³') :- term_sup(23, Sup).

test('capitalize(hELlO)', Cap == 'Hello') :- capitalize(hELlO, Cap).
test('capitalize(h)', Cap == 'H') :- capitalize(h, Cap).

:- end_tests(utils).
