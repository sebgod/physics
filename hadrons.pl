:- module(hadrons, [classical_hadron/1]).

:- use_module(utils, [call_semidet_ground/2 as hadrons_call_semidet]).
:- use_module(baryons, [classical_baryon/1]).
:- use_module(mesons, [classical_meson/1]).

:- meta_predicate hadrons_call_semidet(1, ?).

classical_hadron(P) :- hadrons_call_semidet(classical_hadron_nd, P).

classical_hadron_nd(P) :- classical_baryon(P).
classical_hadron_nd(P) :- classical_meson(P).
