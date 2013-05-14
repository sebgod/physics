:- module(vector, [
                   dimension/2,
                   standard_base/3,
                   scale/3,
                   norm/2,
                   normalized/2,
                   sum/3,
                   difference/3,
                   inner_product/3,
                   outer_product/3,
                   cartesian_sign/2,
                   quadrant/2,
                   coordinate_transform/2
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

cartesian_sign(cv(X, Y), sign(SX, SY)) :-
    SX is sign(X),
    SY is sign(Y).
sign_quadrant(sign(0, 0), origin).
sign_quadrant(sign(+1, +1), q(i)).
sign_quadrant(sign(-1, +1), q(ii)).
sign_quadrant(sign(-1, -1), q(iii)).
sign_quadrant(sign(+1, -1), q(iv)).

quadrant(CV, Q) :-
    cartesian_sign(CV, Sign),
    once(sign_quadrant(Sign, Q)).

coordinate_transform(A, B) :-
    var(A),
    ground(B),
    !,
    coordinate_transform_ft(B, A).

coordinate_transform(A, B) :- coordinate_transform_ft(A, B).

coordinate_transform_ft(cv(X, Y), pv(R, Theta)) :-
    norm(cv(X, Y), R),
    Theta is 2 * atan(Y / (X + R)).

coordinate_transform_ft(pv(R, Theta), cv(X, Y)) :-
    X is R * cos(Theta),
    Y is R * sin(Theta).

coordinate_transform_ft(cv(X, Y, Z), is(R, I, A)) :-
    norm(cv(X, Y, Z), R),
    I is acos(Z / R),
    A is atan2(Y, X).

coordinate_transform_ft(is(R, I, A), cv(X, Y,Z)) :-
    SinI is sin(I),
    X is R * SinI * cos(A),
    Y is R * SinI * sin(A),
    Z is R * cos(I).

:- begin_tests(vector).

test('sign(+,+)') :- cartesian_sign(cv(1,1), sign(+1,+1)).
test('sign(+,-)') :- cartesian_sign(cv(1,-1), sign(+1,-1)).
test('sign(0,0)') :- cartesian_sign(cv(0,0), sign(0,0)).

test('quadrant(cv(1,1), q(i))') :- quadrant(cv(1,1), q(i)).
test('quadrant(cv(-1,1), q(ii))') :- quadrant(cv(1,1), q(i)).
test('quadrant(cv(-1,-1), q(iii))') :- quadrant(cv(1,1), q(i)).
test('quadrant(cv(1,-1), q(iv))') :- quadrant(cv(1,1), q(i)).
test('quadrant(cv(0,0), origin)') :- quadrant(cv(0,0), origin).

test('coordinate_transform(cv-pv)',
     setup((X0 = 1000, Y0 = -1000))) :-
    coordinate_transform(cv(X0, Y0), PV),
    coordinate_transform(PV, cv(X1,Y1)),
    RoundX is round(X1),
    RoundY is round(Y1),
    assertion(RoundX =:= X0),
    assertion(RoundY =:= Y0).

test('coordinate_transform(cv-is)',
     setup((X0 = 1000, Y0 = -1000, Z0 = 1000))
    ) :-
    coordinate_transform(cv(X0, Y0, Z0), SV),
    coordinate_transform(SV, cv(X1,Y1, Z1)),
    RoundX is round(X1),
    RoundY is round(Y1),
    RoundZ is round(Z1),
    assertion(RoundX =:= X0),
    assertion(RoundY =:= Y0),
    assertion(RoundZ =:= Z0).


:- end_tests(vector).
