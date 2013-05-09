:- module(baryons, [
                    classical_baryon/1,
                    nucleon/1,
                    quark_content/4
                   ]).
:- use_module(utils).
:- use_module(particles, [proper_anti_particle/2]).
:- use_module(color_charge, [white/3, primary_color/3, anti_color/3]).
:- use_module(quarks, []).

%%	baryon(+Particle) is semidet.
%%  baryon(?Particle) is nondet.
classical_baryon(P) :- ground_semidet(P, classical_baryon_nd).
classical_baryon_nd(P) :- nucleon(P).
classical_baryon_nd(P) :- lambda(P).

nucleon_type(proton).
nucleon_type(neutron).

nucleon(P) :-  nucleon_type(P).
nucleon(anti(P)) :- nucleon_type(P).

lambda_third_quark(strange).
lambda_third_quark(charm).
lambda_third_quark(bottom).
lambda_third_quark(top).

lambda(lambda(Q)) :-       lambda_third_quark(Q).
lambda(anti(lambda(Q))) :- lambda_third_quark(Q).

quark_content_flavour(proton, up, up, down).
quark_content_flavour(neutron, up, down, down).
quark_content_flavour(lambda(Quark), up, down, Quark) :-
    lambda_third_quark(Quark).

quark_number(QuantumNumber, QuarkFlavour, ValueN) :-
    primary_color(Quark, QuarkFlavour, _),
    (   particles:quantum_number(QuantumNumber, Quark, Value0)
    ->  ValueN = Value0
    ;   ValueN = 0
    ),
    !.

particles:quantum_number_mf(QuantumNumber, Baryon, Value) :-
    classical_baryon(Baryon),
    particles:quantum_number(QuantumNumber),
    quark_content_flavour(Baryon, QF1, QF2, QF3),
    quark_number(QuantumNumber, QF1, FN1),
    quark_number(QuantumNumber, QF2, FN2),
    quark_number(QuantumNumber, QF3, FN3),
    Value is FN1 + FN2 + FN3.

quark_content(AntiBaryon, Q1, Q2, Q3) :-
    proper_anti_particle(Baryon, AntiBaryon),
    quark_content_flavour(Baryon, Fl1, Fl2, Fl3),
    white(C1, C2, C3),
    anti_color(Q1, Fl1, C1),
    anti_color(Q2, Fl2, C2),
    anti_color(Q3, Fl3, C3).

quark_content(Baryon, Q1, Q2, Q3) :-
    quark_content_flavour(Baryon, Fl1, Fl2, Fl3),
    white(C1, C2, C3),
    primary_color(Q1, Fl1, C1),
    primary_color(Q2, Fl2, C2),
    primary_color(Q3, Fl3, C3).



