:- module(bosons, [
                   boson/1,
                   elementary_boson/1,
                   gauge_boson/1,
                   interaction/2
                  ]).

:- use_module(particles, []).
:- use_module(color_charge, [gluon_color/1]).

elementary_boson_type(photon).
elementary_boson_type(w_boson).
elementary_boson_type(z_boson).
elementary_boson_type(gluon(Color)) :-
    gluon_color(Color).
elementary_boson_type(higgs).
elementary_boson_type(graviton).

elementary_boson(anti(w_boson)).
elementary_boson(P) :- elementary_boson_type(P).

boson(P) :- particles:spin(P, 1).

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

particles:quantum_number(electric_charge, w_boson, -1).

particles:color_charge(gluon(GluonColor), GluonColor) :-
    gluon_color(GluonColor).

particles:spin(photon, 1).
particles:spin(w_boson, 1).
particles:spin(z_boson, 1).
particles:spin(gluon(C), 1) :- gluon_color(C).
particles:spin(higgs, 0).
particles:spin(graviton, 2).

particles:symbol(photon, 'γ').
particles:symbol(w_boson, 'W\u207B').
particles:symbol(anti(w_boson), 'W⁺').
particles:symbol(z_boson, 'Z').
particles:symbol(gluon(C), g) :- gluon_color(C).
particles:symbol(higgs, 'H\u2070').
particles:symbol(graviton, 'G').
