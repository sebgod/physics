:- module(complex_number, [
                           normalize_term/2,
                           real/2,
                           imaginary/2,
                           conjugate/2,
                           cartesian_vector/2
                          ]).

normalize_term(Z, complex(R, 0)) :-
    \+ compound(Z),
    Z \= i,
    (   ground(Z) -> R is Z ; R = Z ).

normalize_term(A * B, complex(R, I)) :-
    compound(A),
    compound(B),
    normalize_term(A, complex(AR, AI)),
    normalize_term(B, complex(BR, BI)),
    !,
    R is AR * BR,
    I is AI * BI.

normalize_term(A + B, complex(R, I)) :-
    compound(A),
    compound(B),
    normalize_term(A, complex(AR, AI)),
    normalize_term(B, complex(BR, BI)),
    !,
    R is AR + BR,
    I is AI + BI.

normalize_term(A - B, complex(R, I)) :-
    compound(A),
    compound(B),
    normalize_term(A, complex(AR, AI)),
    normalize_term(B, complex(BR, BI)),
    !,
    R is AR - BR,
    I is AI - BI.

normalize_term(i, complex(0, 1)).

normalize_term(R0 + I0 * i, complex(R1, I1)) :-
    R1 is R0,
    I1 is I0.
normalize_term(R0 - I0 * i, complex(R1, I1)) :-
    R1 is R0,
    I1 is -I0.
normalize_term(i * A, complex(0, I)) :- I is A.
normalize_term(A * i, complex(0, I)) :- I is A.

normalize_term(-Z, complex(R1, I1)) :-
    normalize_term(Z, complex(R0, I0)),
    R1 is -R0,
    I1 is -I0.

normalize_term(complex(R, I), complex(R, I)).

real(Z, R)  :- normalize_term(Z, complex(R, _I)).
imaginary(Z, I) :- normalize_term(Z, complex(_R, I)).

conjugate(Z, complex(R, I1)) :-
    normalize_term(Z, complex(R, I0)),
    I1 is -I0.

cartesian_vector(Z, cv(R, I)) :-
    real(Z, R),
    imaginary(Z, I).


:- begin_tests(complex_number).

test('normalize_term(4)') :-
    normalize_term(4, complex(4, 0)).

test('normalize_term(i)') :-
    normalize_term(i, complex(0, 1)).

test('normalize_term(4 + 3 * i)') :-
    normalize_term(4 + 3 * i, complex(4, 3)).

test('normalize_term(4 - 3 * i)') :-
    normalize_term(4 - 3 * i, complex(4, -3)).

test('normalize_term(-(4 - 3 * i))') :-
    normalize_term(-(4 - 3 * i), complex(-4, 3)).

test('normalize_term(complex(4, 3))') :-
    normalize_term(complex(4, 3), complex(4, 3)).

test('normalize_term(A - B)') :-
    normalize_term(complex(1, 1) - complex(1, 1), complex(0, 0)).

test('normalize_term(A + B)') :-
    normalize_term(complex(1, 2) + complex(3, 4), complex(4, 6)).

test('normalize_term(A * B)') :-
    normalize_term(complex(2, 3) * complex(4, 5), complex(8, 15)).

test(real) :- real(complex(pi, 0), pi).

test(imaginary) :- imaginary(complex(0, pi), pi).

test(conjugate) :- conjugate(complex(3, 4), complex(3, -4)).

:- end_tests(complex_number).
