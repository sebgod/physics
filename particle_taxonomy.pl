:- module(particle_taxanomoy, []).

:- use_module(particles, []).
:- use_module(fermions, [fermion/1]).
:- use_module(bosons, [elementary_boson/1]).
:- use_module(hadrons, [classical_hadron/1]).


particles:elementary(P) :- elementary_boson(P).
particles:elementary(P) :- fermion(P).

particles:combined(P) :- classical_hadron(P).
