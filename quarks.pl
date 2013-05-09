:- module(quarks, [
                   quark/1,
                   flavour/1,
                   quark_type/2
                 ]).

:- use_module(particles).
:- use_module(color_charge).

flavour(up).
flavour(down).
flavour(charm).
flavour(strange).
flavour(top).
flavour(bottom).

quark_color(Color) :- primary_color(Color).

quark(Q) :-
    (   compound(Q) ->  Q \= anti(_) ; flavour(F) ),
    color(Q, F, C),
    quark_color(C).

quark(anti(Q)) :-
    (   compound(Q) ->  true ; flavour(F) ),
    anti_color(anti(Q), F, _).

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

particles:quantum_number(FlavourNumber, Quark, 0) :-
    flavour(FlavourNumber),
    flavour(QuarkFlavour),
    QuarkFlavour \= FlavourNumber,
    color(Quark, QuarkFlavour, _Color).

particles:quantum_number(charm, charm(C), 1) :-      quark_color(C).
particles:quantum_number(strange, strange(C), -1) :- quark_color(C).
particles:quantum_number(top, top(C), 1) :-          quark_color(C).
particles:quantum_number(bottom, bottom(C), -1) :-   quark_color(C).

particles:quantum_number(electric_charge, up(C), +2 rdiv 3) :-
    quark_color(C).
particles:quantum_number(electric_charge, down(C), -1 rdiv 3) :-
    quark_color(C).
particles:quantum_number(electric_charge, charm(C), +2 rdiv 3) :-
    quark_color(C).
particles:quantum_number(electric_charge, strange(C), -1 rdiv 3) :-
    quark_color(C).
particles:quantum_number(electric_charge, top(C), +2 rdiv 3) :-
    quark_color(C).
particles:quantum_number(electric_charge, bottom(C), -1 rdiv 3) :-
    quark_color(C).

particles:quantum_number(isospin, top(C), +1 rdiv 2) :-
    quark_color(C).
particles:quantum_number(isospin, down(C), -1 rdiv 2) :-
    quark_color(C).

particles:quantum_number(baryon, Quark, +1 rdiv 3) :-
    (   ground(Quark) -> ! ; true ),
    flavour(QuarkFlavour),
    primary_color(Quark, QuarkFlavour, _Color).

particles:symbol(AntiQuark, AntiSymbol) :-
    proper_anti_particle(Quark, AntiQuark),
    particles:symbol(Quark, QuarkSymbol),
    atom_concat('\u0305', QuarkSymbol, AntiSymbol).

particles:symbol(up(_), u).
particles:symbol(down(_), d).
particles:symbol(charm(_), c).
particles:symbol(strange(_), s).
particles:symbol(top(_), t).
particles:symbol(bottom(_), b).




