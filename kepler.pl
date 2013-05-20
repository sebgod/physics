:- module(kepler, [
                   first_law/4,
                   semi_major_axis_semi_latus_rectum/3
                  ]).

motion_type(cicular, 0).
motion_type(hyperbolic, Eccentricity) :-
    Eccentricity >= 2.

motion_type(parabolic, Eccentricity) :-
    Eccentricity >= 1,
    Eccentricity < 2.

motion_type(elliptic, Eccentricity) :-
    Eccentricity > 0,
    Eccentricity < 1.

semi_major_axis_semi_latus_rectum(SemiMajorAxis, E, SemiLatusRectum) :-
    E1 is 1 - E*E,
    (   var(SemiMajorAxis) -> SemiMajorAxis is SemiLatusRectum / E1
    ;   var(SemiLatusRectum) -> SemiLatusRectum is SemiMajorAxis * E1
    ;   SemiMajorAxis * E1 =:= SemiLatusRectum
    ).

first_law(SemiLatusRectum, Eccentricity, Radius, Phase) :-
    var(Radius),
    Eccentricity >= 0,
    Radius is SemiLatusRectum / (1 + Eccentricity * cos(Phase)).

:- begin_tests(kepler).

test(parabolic, [setup(member(E, [1, 1.5, 1.999999]))]) :-
    motion_type(parabolic, E).
test(cicular) :-    motion_type(cicular, 0).
test(elliptic, [setup(member(E, [0.0001, 0.5, 0.999999]))]) :-
    motion_type(elliptic, E).
test(hyperbolic, [setup(member(E, [2, 2.001, 3]))]) :-
    motion_type(hyperbolic, E).

test('first_law(var(Radius))',
     [setup((E = 0.2, SMA = 1.496 * 1e8)),
      Result =:= 1.224]) :-
    semi_major_axis_semi_latus_rectum(SMA, E, SLR),
    first_law(SLR, E, Radius, pi rdiv 6),
    Result is round(Radius / 1e5) / 1e3.

:- end_tests(kepler).

