:- module(leptons, [lepton/1]).

:- use_module(quantum_numbers, []).

kind(electron).
kind(muon).
kind(tau).

neutrino(neutrino(F)) :-kind(F).

lepton_flavour(L) :- kind(L).
lepton_flavour(L) :- neutrino(L).

lepton(L) :- lepton_flavour(L).
lepton(anti(L)) :- lepton_flavour(L).

quantum_numbers:quantum_number_mf(lepton, L, 1) :- lepton_flavour(L).

quantum_numbers:quantum_number_mf(electric_charge, F, -1) :- kind(F).
quantum_numbers:quantum_number_mf(electric_charge, neutrino(L), 0) :-
    kind(L).
