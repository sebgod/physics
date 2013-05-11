:- module(utils, [
                  ground_semidet/2,
                  safe_is/2,
                  numeric_inverse/2
                 ]).
:- meta_predicate ground_semidet(?,1).

ground_semidet(Var, Goal) :-
    (   ground(Var)
    ->  call(Goal, Var), !
    ;   call(Goal, Var)
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

numeric_inverse(-A, B) :-
    numeric_inverse_(B, -A).
    %var(A),
    %number(B),
    %A is -B.

numeric_inverse_(A, -B) :-
    number(A),
    var(B),
    B is -A.


:- begin_tests(utils).

test('safe_is(0,1)', [fail]) :- safe_is(0, 1).
test('safe_is(A, 1)') :- safe_is(A, 1), A == 1.
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

:- end_tests(utils).
