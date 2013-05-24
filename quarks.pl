:- module(quarks, [
                   quark/1,
                   flavour/1,
                   quark_type/2
                 ]).

:- use_module(utils, [call_semidet_ground/2 as quark_call_semidet]).
:- use_module(particles, [proper_anti_particle/2 as anti_quark]).
:- use_module(color_charge).
:- use_module(quantum_numbers, [quantum_number/3]).
:- use_module(symbols, []).

:- meta_predicate quark_call_semidet(1, ?).

flavour(up).
flavour(down).
flavour(charm).
flavour(strange).
flavour(top).
flavour(bottom).

quark_color(Color) :- primary_color(Color).

quark(Q) :- quark_call_semidet(quark_nd, Q).

quark_nd(anti(Q)) :-
    (   compound(Q) ->  true ; flavour(F) ),
    anti_color(anti(Q), F, _).
quark_nd(Q) :-
    compound(Q),
    ground(Q),
    !,
    color(Q, F, C),
    flavour(F),
    quark_color(C).
quark_nd(Q) :-
    flavour(F),
    primary_color(Q, F, C),
    quark_color(C).

color_charge:color_charge_mf(Q, Color) :-
    functor(Q, F, Args),
    Args > 0,
    flavour(F),
    color(Q, F, Color).

quark_type(Q, Type) :-
    quark(Q),
    quantum_number(electric_charge, Q, Charge),
    ChargeValue is Charge,
    (   ChargeValue >= 0
    ->  Type = up_type
    ;   Type = down_type
    ).

quantum_numbers:quantum_number_mf(FlavourNumber, Quark, 0) :-
    flavour(FlavourNumber),
    flavour(QuarkFlavour),
    QuarkFlavour \= FlavourNumber,
    color(Quark, QuarkFlavour, _Color).

quantum_numbers:quantum_number_mf(charm, charm(C), 1) :-
    quark_color(C).
quantum_numbers:quantum_number_mf(strange, strange(C), -1) :-
    quark_color(C).
quantum_numbers:quantum_number_mf(top, top(C), 1) :-
    quark_color(C).
quantum_numbers:quantum_number_mf(bottom, bottom(C), -1) :-
    quark_color(C).

quantum_numbers:quantum_number_mf(electric_charge, up(C), +2 rdiv 3) :-
    quark_color(C).
quantum_numbers:quantum_number_mf(electric_charge, down(C), -1 rdiv 3) :-
    quark_color(C).
quantum_numbers:quantum_number_mf(electric_charge, charm(C), +2 rdiv 3) :-
    quark_color(C).
quantum_numbers:quantum_number_mf(electric_charge, strange(C), -1 rdiv 3) :-
    quark_color(C).
quantum_numbers:quantum_number_mf(electric_charge, top(C), +2 rdiv 3) :-
    quark_color(C).
quantum_numbers:quantum_number_mf(electric_charge, bottom(C), -1 rdiv 3) :-
    quark_color(C).

quantum_numbers:quantum_number_mf(isospin, top(C), +1 rdiv 2) :-
    quark_color(C).
quantum_numbers:quantum_number_mf(isospin, down(C), -1 rdiv 2) :-
    quark_color(C).

quantum_numbers:quantum_number_mf(baryon, Quark, +1 rdiv 3) :-
    quark(Quark).

symbols:symbol_mf(AntiQuark, AntiSymbol) :-
    anti_quark(Quark, AntiQuark),
    symbols:symbol(Quark, QuarkSymbol),
    atom_concat('\u0305', QuarkSymbol, AntiSymbol).

symbols:symbol_mf(up(_), u).
symbols:symbol_mf(down(_), d).
symbols:symbol_mf(charm(_), c).
symbols:symbol_mf(strange(_), s).
symbols:symbol_mf(top(_), t).
symbols:symbol_mf(bottom(_), b).




