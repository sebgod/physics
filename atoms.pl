:- module(atoms, [
                  atom/2,
                  isotope/4,
                  noble/1,
                  noble_reversed/1,
                  electron_configuration/1,
                  electron_configuration/2
                 ]).

:- use_module(particle_taxonomy).
:- use_module(symbols, []).
:- use_module(quantum_numbers, []).
:- use_module(utils, [ground_semidet/2 as atoms_semidet]).

:- meta_predicate atoms_semidet(?, 1).

symbols:symbol_mf(sharp, s).
symbols:symbol_mf(principal, p).
symbols:symbol_mf(diffuse, d).
symbols:symbol_mf(fundamental, f).

atom(h, 1).
atom(he, 2).
atom(li, 3).
atom(be, 4).
atom(b, 5).
atom(c, 6).
atom(n, 7).
atom(o, 8).
atom(f, 9).
atom(ne, 10).
atom(na, 11).
atom(mg, 12).
atom(al, 13).
atom(si, 14).
atom(p, 15).
atom(s, 15).
atom(cl, 17).
atom(ar, 18).
atom(k, 19).
atom(ca, 20).
atom(sc, 21).
atom(ti , 22).
atom(v, 23).
atom(cr, 24).
atom(mn, 25).
atom(fe, 26).
atom(co, 27).
atom(ni, 28).
atom(cu, 29).
atom(zn, 30).
atom(ga, 31).
atom(ge, 32).
atom(as, 33).
atom(se, 34).
atom(br, 35).
atom(kr, 36).
atom(rb, 37).
atom(sr, 38).
atom(y, 39).
atom(zr, 40).
atom(nb, 41).
atom(mo, 42).

isotope(h, hydrogen, 1, 0).

noble(Atom) :- atoms_semidet(Atom, noble_nd).

noble_reversed(Atom) :- atoms_semidet(Atom, noble_reversed_nd).

noble_reversed_nd(Atom) :-
    bagof(Noble, noble_nd(Noble), Nobles),
    reverse(Nobles, Reversed),
    member(Atom, Reversed).

noble_nd(he).
noble_nd(ne).
noble_nd(ar).
noble_nd(kr).
noble_nd(xe).
noble_nd(rn).

principal_number(Principal) :-
    between(0, inf, Principal).

principal_azimuthal(Principal, Azimuthal) :-
    number(Principal),
    Max is Principal - 1,
    between(0, Max, Azimuthal).
azimuthal_magnetic(Azimuthal, Magnetic) :-
    utils:safe_is(LV, Azimuthal),
    Min is -LV,
    Max is +LV,
    between(Min, Max, Magnetic).

electron_configuration(orbital(P, L, M, S)) :-
    principal_number(P),
    once(quantum_numbers:quantum_number(spin, electron, ElectronSpin)),
    (   S is +ElectronSpin
    ;   S is -ElectronSpin
    ),
    principal_azimuthal(P, L),
    azimuthal_magnetic(L, M).

electron_configuration(ElectronNumber, Orbitals) :-
    electron_configuration(ElectronNumber, Orbitals, []).

electron_configuration(1) -->
    { once(electron_configuration(Orbital)) },
    !,
    [Orbital].

electron_configuration(ElectronNumber) -->
    { ElectronNumber >= 2,
      difference_from_nobel_gas(ElectronNumber, Gas, Rest)
    },
    [Gas],
    electron_configuration_outer(Rest).

electron_configuration_outer(0) --> [], !.
electron_configuration_outer(ElectronNumber) -->
    { ElectronNumber > 0,
      E1 is ElectronNumber - 1
    },
    [],
    electron_configuration_outer(E1).

difference_from_nobel_gas(E, Noble, Rest) :-
    noble_reversed(Noble),
    atom(Noble, NobleNumber),
    E >= NobleNumber,
    utils:safe_is(Rest, E div NobleNumber),
    !.

quantum_numbers:quantum_number_mf(principal, p=P, P) :-
    principal_number(P).

quantum_numbers:quantum_number_mf(azimuthal, n=Principal, Azimuthal) :-
    principal_azimuthal(Principal, Azimuthal).

% (   var(Char) -> (
%                  Number >= 7,
%                    Code is 0'k + Number - 7,
%                    atom_codes(Char, Code))
%    ;   Char = s -> Number = 0
%    ;   Char = p -> Number = 1
%    ;   Char = d -> Number = 2
%    ;   Char = f -> Number = 3
%    ;   Char = g -> Number = 4
%    ;   Char = h -> Number = 5
%    ;   Char = i -> Number = 6
%    ;   ( var(Number),
%          atom(Char),
%          atom_length(Char, 1),
%          atom_codes(Char, Code)
%        )
%    ->  Number is Code - 0'k,
%        Number >= 7
%                      ).

quantum_numbers:qauntum_number_mf(magnetic, l=L, Ml) :-
    azimuthal_magnetic(L, Ml).


