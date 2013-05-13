:- module(hadrons, [classical_hadron/1]).

:- use_module(utils, [ground_semidet/2]).
:- use_module(baryons, [classical_baryon/1]).
:- use_module(mesons, [classical_meson/1]).

classical_hadron(P) :- ground_semidet(P, classical_hadron_nd).

classical_hadron_nd(P) :- classical_baryon(P).
classical_hadron_nd(P) :- classical_meson(P).
