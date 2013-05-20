:- module(bosons, [
                   boson/1,
                   elementary_boson/1,
                   gauge_boson/1,
                   interaction/2
                  ]).

:- use_module(particles, []).
:- use_module(quantum_numbers, [quantum_number/3]).
:- use_module(color_charge, [gluon_color/1]).
:- use_module(symbols, []).

elementary_boson_type(photon).
elementary_boson_type(w_boson).
elementary_boson_type(z_boson).
elementary_boson_type(gluon(Color)) :-
    gluon_color(Color).
elementary_boson_type(higgs).
elementary_boson_type(graviton).

elementary_boson(anti(w_boson)).
elementary_boson(P) :- elementary_boson_type(P).

boson(P) :- quantum_number(spin, P, 1).

gauge_boson(photon).
gauge_boson(w_boson).
gauge_boson(anti(w_boson)).
gauge_boson(z_boson).
gauge_boson(gluon(C)) :- gluon_color(C).

interaction(photon, electromagnetic).
interaction(w_boson, weak).
interaction(anti(w_boson), weak).
interaction(z_boson, weak).
interaction(gluon(C), strong) :- gluon_color(C).
interaction(graviton, gravity).

particles:anti_particle_self(photon).
particles:anti_particle_self(z_boson).
particles:anti_particle_self(gluon(C)) :- gluon_color(C).
particles:anti_particle_self(higgs).
particles:anti_particle_self(graviton).

quantum_numbers:quantum_number_mf(electric_charge, w_boson, -1).

color_charge:color_charge_mf(gluon(GluonColor), GluonColor) :-
    gluon_color(GluonColor).

quantum_numbers:quantum_number_mf(spin, photon, 1).
quantum_numbers:quantum_number_mf(spin, w_boson, 1).
quantum_numbers:quantum_number_mf(spin, z_boson, 1).
quantum_numbers:quantum_number_mf(spin, gluon(C), 1) :- gluon_color(C).
quantum_numbers:quantum_number_mf(spin, higgs, 0).
quantum_numbers:quantum_number_mf(spin, graviton, 2).

symbols:symbol_mf(photon, 'γ').
symbols:symbol_mf(w_boson, 'W\u207B').
symbols:symbol_mf(anti(w_boson), 'W⁺').
symbols:symbol_mf(z_boson, 'Z').
symbols:symbol_mf(gluon(C), g) :- gluon_color(C).
symbols:symbol_mf(higgs, 'H\u2070').
symbols:symbol_mf(graviton, 'G').
