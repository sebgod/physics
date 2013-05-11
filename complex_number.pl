:- module(complex_number, [
                           normalize_term/2,
                           real/2,
                           imaginary/2,
                           conjugate/2,
                           cartesian_vector/2
                          ]).

:- use_module(utils, [safe_is/2]).

normalize_term(Z, complex(R1, I1)) :-
    normalize_term_(Z, complex(R0, I0)),
    safe_is(R1, R0),
    safe_is(I1, I0).

normalize_term_(Z, complex(Z, 0)) :-
    \+ compound(Z),
    Z \= i.

normalize_term_(A * B, complex(AR * BR, AI * BI)) :-
    binary_expr(A, B, complex(AR, AI), complex(BR, BI)),
    !.

normalize_term_(A + B, complex(AR + BR, AI + BI)) :-
    binary_expr(A, B, complex(AR, AI), complex(BR, BI)),
    !.

normalize_term_(A - B, complex(AR - BR, AI - BI)) :-
    binary_expr(A, B, complex(AR, AI), complex(BR, BI)),
    !.

normalize_term_(i, complex(0, 1)).

normalize_term_(R + I * i, complex(R, I)).
normalize_term_(R - I * i, complex(R, -I)).

normalize_term_(i * I, complex(0, I)).
normalize_term_(I * i, complex(0, I)).

normalize_term_(-Z, complex(-R, -I)) :-
    normalize_term_(Z, complex(R, I)).

normalize_term_(complex(R, I), complex(R, I)).

binary_expr(A, B, AZ, BZ) :-
    compound(A),
    compound(B),
    normalize_term_(A, AZ),
    normalize_term_(B, BZ).

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

test('normalize_term(R - I * i, complex(4, -3))') :-
    normalize_term(R - I * i, complex(4, -3)),
    R is 4,
    I is 3.

test(real) :- real(complex(pi, 0), pi).

test(imaginary) :- imaginary(complex(0, pi), pi).

test(conjugate) :- conjugate(complex(3, 4), complex(3, -4)).

:- end_tests(complex_number).
