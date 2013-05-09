:- module(utils, [ground_semidet/2]).
:- meta_predicate ground_semidet(?,1).

ground_semidet(Var, Goal) :-
    (   ground(Var)
    ->  call(Goal, Var), !
    ;   call(Goal, Var)
    ).
