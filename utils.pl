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

safe_is(A, B) :-
    (   A == B -> A = B
    ;   ground(B) -> A is B
    ;   ground(A), var(B) -> B is A
    ;   numeric_inverse(A, B)
    ).

numeric_inverse(A, -B) :-
    number(A),
    var(B),
    B is -A.
