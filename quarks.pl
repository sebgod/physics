:- module(quarks, [
                   quark/1,
                   flavour/1,
                   quark_type/2
                 ]).

:- use_module(utils, [ground_semidet/2 as quark_semidet]).
:- use_module(particles, [proper_anti_particle/2 as anti_quark]).
:- use_module(color_charge).

:- meta_predicate quark_semidet(?,1).

flavour(up).
flavour(down).
flavour(charm).
flavour(strange).
flavour(top).
flavour(bottom).

quark_color(Color) :- primary_color(Color).

quark(anti(Q)) :-
    (   compound(Q) ->  true ; flavour(F) ),
    anti_color(anti(Q), F, _).
quark(Q) :-
    compound(Q),
    ground(Q),
    !,
    color(Q, F, C),
    flavour(F),
    quark_color(C).
quark(Q) :-
    flavour(F),
    primary_color(Q, F, C),
    quark_color(C).

particles:color_charge(Q, Color) :-
    functor(Q, F, Args),
    Args > 0,
    flavour(F),
    color(Q, F, Color).

quark_type(Q, Type) :-
    quark(Q),
    particles:quantum_number(electric_charge, Q, Charge),
    ChargeValue is Charge,
    (   ChargeValue >= 0
    ->  Type = up_type
    ;   Type = down_type
    ).

particles:quantum_number_mf(FlavourNumber, Quark, 0) :-
    flavour(FlavourNumber),
    flavour(QuarkFlavour),
    QuarkFlavour \= FlavourNumber,
    color(Quark, QuarkFlavour, _Color).

particles:quantum_number_mf(charm, charm(C), 1) :-      quark_color(C).
particles:quantum_number_mf(strange, strange(C), -1) :- quark_color(C).
particles:quantum_number_mf(top, top(C), 1) :-          quark_color(C).
particles:quantum_number_mf(bottom, bottom(C), -1) :-   quark_color(C).

particles:quantum_number_mf(electric_charge, up(C), +2 rdiv 3) :-
    quark_color(C).
particles:quantum_number_mf(electric_charge, down(C), -1 rdiv 3) :-
    quark_color(C).
particles:quantum_number_mf(electric_charge, charm(C), +2 rdiv 3) :-
    quark_color(C).
particles:quantum_number_mf(electric_charge, strange(C), -1 rdiv 3) :-
    quark_color(C).
particles:quantum_number_mf(electric_charge, top(C), +2 rdiv 3) :-
    quark_color(C).
particles:quantum_number_mf(electric_charge, bottom(C), -1 rdiv 3) :-
    quark_color(C).

particles:quantum_number_mf(isospin, top(C), +1 rdiv 2) :-
    quark_color(C).
particles:quantum_number_mf(isospin, down(C), -1 rdiv 2) :-
    quark_color(C).

particles:quantum_number_mf(baryon, Quark, +1 rdiv 3) :-
    quark_semidet(Quark, quark).

particles:symbol(AntiQuark, AntiSymbol) :-
    anti_quark(Quark, AntiQuark),
    particles:symbol(Quark, QuarkSymbol),
    atom_concat('\u0305', QuarkSymbol, AntiSymbol).

particles:symbol(up(_), u).
particles:symbol(down(_), d).
particles:symbol(charm(_), c).
particles:symbol(strange(_), s).
particles:symbol(top(_), t).
particles:symbol(bottom(_), b).




