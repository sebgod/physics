:- module(leptons, [lepton/1]).

:- use_module(particles, []).

flavour(electron).
flavour(neutrino(electron)).
flavour(muon).
flavour(neutrino(muon)).
flavour(tau).
flavour(neutrino(tau)).

lepton(L) :- flavour(L).
lepton(anti(L)) :- flavour(L).

particles:quantum_number_mf(lepton, L, 1) :- flavour(L).

particles:quantum_number_mf(electric_charge, electron, -1).
particles:quantum_number_mf(electric_charge, muon, -1).
particles:quantum_number_mf(electric_charge, tau, -1).

particles:quantum_number_mf(electric_charge, neutrino(L), 0) :-
    flavour(L),
    L \= neutrino(_).
