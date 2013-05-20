:- module(lorentz_group, [
                          lorentz_covariant/1,
                          lorentz_covariant_mf/1]).

:- multifile lorentz_covariant_mf/1.

lorentz_covariant(Q) :- lorentz_covariant_mf(Q).
