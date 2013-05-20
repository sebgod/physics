:- module(fermions, [
                     fermion/1
                    ]).

:- use_module(particles, []).
:- use_module(quarks, [quark/1]).
:- use_module(leptons, [lepton/1]).
:- use_module(quantum_numbers, []).

fermion(P) :-
    (   ground(P)
    ->  fermion_nd(P), !
    ;   fermion_nd(P)
    ).

fermion_nd(F) :- quark(F).
fermion_nd(L) :- lepton(L).

quantum_numbers:quantum_number_mf(spin, P, 1 rdiv 2) :- fermion(P).
