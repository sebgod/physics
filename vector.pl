:- module(vector, [
                   dimension/2,
                   standard_base/3,
                   scale/3,
                   norm/2,
                   normalized/2,
                   sum/3,
                   difference/3,
                   inner_product/3,
                   outer_product/3
                  ]).

%%	dimension(+CartVector, -Dim) is det.
%%	dimension(+CartVector, +Dim) is semidet.
dimension(CV, Dim) :-
    functor(CV, cv, Dim),
    Dim >= 1.

%%	standard_base(?Dim:integer, ?Symbol, ?CartVector) is nondet.

standard_base(Dim, e(N), CV) :-
    \+ standard_base_spatial(Dim, e(N), CV),
    !,
    dimension(CV, Dim),
    Dim > 3,
    between(1, Dim, N),
    standard_base_component(N, CV, Dim).

standard_base(Dim, E, CV) :-
    (   ground(Dim), ground(E) ; ground(CV)
    ->  standard_base_spatial(Dim, E, CV), !
    ;   standard_base_spatial(Dim, E, CV)
    ).

standard_base_spatial(1, e(1), cv(1)).

standard_base_spatial(2, e(1), cv(1, 0)).
standard_base_spatial(2, e(2), cv(0, 1)).

standard_base_spatial(3, e(1), cv(1, 0, 0)).
standard_base_spatial(3, e(2), cv(0, 1, 0)).
standard_base_spatial(3, e(3), cv(0, 0, 1)).

standard_base_component(N, CV, Component) :-
    Component > 0,
    !,
    (   Component == N
    ->  arg(N, CV, 1)
    ;   arg(Component, CV, 0)
    ),
    ComponentS1 is Component - 1,
    standard_base_component(N, CV, ComponentS1).

standard_base_component(_N, _CV, 0) :- !.

%%	scale(+CartVector, +ScaleFactor, -Result) is det.
scale(cv(X0, Y0, Z0), A, cv(X1, Y1, Z1)) :-
    X1 is A*X0,
    Y1 is A*Y0,
    Z1 is A*Z0.

%%	length(+CartVector, -Length:integer) is det.
norm(cv(X, Y), Length) :- norm(cv(X, Y, 0), Length).

norm(cv(X, Y, Z), Length) :-
    Length is (X**2 + Y**2 + Z**2) ** (1 rdiv 2).

%%	normalized(+CartVector, +Normalized) is semidet.
normalized(cv(X0, Y0), cv(X1, Y1)) :-
    normalized(cv(X0, Y0, 0), cv(X1, Y1, 0)).
normalized(cv(X0, Y0, Z0), cv(X1, Y1, Z1)) :-
    norm(cv(X0, Y0, Z0), Norm),
    X1 is X0 rdiv Norm,
    Y1 is Y0 rdiv Norm,
    Z1 is Z0 rdiv Norm.

%%	sum(+CartVectorA, +CartVectorB, +CartVectorSum) is semidet.
% Sum is A + B.
sum(cv(X0, Y0, Z0), cv(X1, Y1, Z1), cv(X2, Y2, Z2)) :-
    X2 is X0 + X1,
    Y2 is Y0 + Y1,
    Z2 is Z0 + Z1.

%%	difference(+CartVectorA, +CartVectorB, +CartVectorDiff) is semidet.
% Diff is A - B.
difference(cv(X0, Y0, Z0), cv(X1, Y1, Z1), cv(X2, Y2, Z2)) :-
    X2 is X0 - X1,
    Y2 is Y0 - Y1,
    Z2 is Z0 - Z1.

inner_product(cv(X0, Y0, Z0), cv(X1, Y1, Z1), InnerProduct) :-
    InnerProduct is X0 * X1 + Y0 * Y1 + Z0 + Z1.

outer_product(cv(A1, A2, A3), cv(B1, B2, B3), pseudo(cv(C1, C2, C3))) :-
    C1 is A2 * B3 - B3 * B2,
    C2 is A3 * B1 - A1 * B3,
    C3 is A1 * B2 - A2 * B1.

