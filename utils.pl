:- module(utils, [
                  call_semidet_ground/2,
                  call_semidet_ground/3,
                  call_semidet_ground_first/3,
                  safe_is/2,
                  numeric_inverse/2,
                  term_sup/2,
                  map_term_codes/3,
                  between2d/4,
                  findnsols/4
                 ]).
:- meta_predicate
    call_semidet_ground(1, ?),
    call_semidet_ground(2, ?, ?),
    call_semidet_ground_first(2, ?,?),
    findnsols(+, ?, :, -),
    findnsols(+, ?, :, -, ?),
    maxsols(?, 0),
    map_term_codes(2, +, ?).

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

%%	safe_is(+A, +B) is semidet.
%%	safe_is(-A, +B) is det.
safe_is(A, B) :-
    (   A == B                -> A = B, _ is A
    ;   ground(B), var(A)     -> A is B
    ;   ground(A), var(B)     -> B is A
    ;   ground(A), ground(B)  -> A =:= B
    ;   numeric_inverse(A, B)
    ).

numeric_inverse(-A, B) :- numeric_inverse_(B, -A), !. % green cut
numeric_inverse(A, -B) :- numeric_inverse_(A, -B).

numeric_inverse_(A, -B) :-
    _ is A,
    var(B),
    B is -A.

term_sup(Term, Symbol) :-
    map_term_codes(term_sup_map, Term, Symbol).

%%	map_term_codes(:Map, +Term, -Symbol) is det.
map_term_codes(Map, Term, Symbol) :-
    term_to_atom(Term, Atom),
    atom_codes(Atom, AtomCodes),
    length(AtomCodes, Length),
    length(SymbolCodes, Length),
    maplist(Map, AtomCodes, SymbolCodes),
    atom_codes(Symbol, SymbolCodes).

term_sup_map(Code, Sup) :-
    (   between(0'4, 0'9, Code) -> Sup is 0x2070 + Code - 0'0
    ;   Code = 0'0 -> Sup = 0x2070
    ;   Code = 0'1 -> Sup = 0'¹
    ;   Code = 0'2 -> Sup = 0'²
    ;   Code = 0'3 -> Sup = 0'³
    ;   Code = 0'+ -> Sup = 0'⁺
    ;   Code = 0'- -> Sup = 0'⁻
    ;   Code = 0'n -> Sup = 0'ⁿ
    ;   Code = 0'= -> Sup = 0x207c
    ;   Code = 0x28 -> Sup = 0x207d
    ;   Code = 0x29 -> Sup = 0x207e
    ;   Code = 0xa0 -> Sup = 0xa0
    ).

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

:- end_tests(utils).
