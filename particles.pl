:- module(particles, [
                      particle/1,
                      anti_particle_self/1,
                      not_anti_particle/1,
                      proper_anti_particle/2,
                      elementary/1,
                      combined/1,
                      color_charge/2,
                      spin/2,
                      symbol/2,
                      quantum_number/3,
                      quantum_number/1,
					  quantum_number_mf/3,
                      conserved_quantum_number/1,
                      flavour_quantum_number/1
                     ]).

:- meta_predicate anti_property(1,+,2,+).
:- multifile anti_particle_self/1.
:- multifile symbol/2.
:- multifile spin/2.
:- multifile color_charge/2.
:- multifile quantum_number_mf/3.
:- multifile elementary/1.
:- multifile combined/1.

:- use_module(utils, [ground_semidet/2]).
:- use_module(color_charge, [color/3, anti_color/3]).


%%	not_anti_particle(+Particle) is semidet.
not_anti_particle(Particle) :- Particle \= anti(_).

%%	quantum_number(+NumberType, +AntiParticle, ?AntiNumber) is semidet.
quantum_number(NumberType, Particle, Number) :-
    ground_semidet(Particle, not_anti_particle),
    quantum_number_mf(NumberType, Particle, Number).

quantum_number(NumberType, anti(AntiParticle), AntiNumber) :-
    proper_anti_particle(Particle, anti(AntiParticle)),
    quantum_number_mf(NumberType, Particle, Number),
    AntiNumber is -Number.

quantum_number(Q) :- ground_semidet(Q, quantum_number_nd).

quantum_number_nd(Q) :- flavour_quantum_number(Q).
quantum_number_nd(Q) :- conserved_quantum_number(Q).

flavour_quantum_number(isospin).
flavour_quantum_number(charm).
flavour_quantum_number(strange).
flavour_quantum_number(top).
flavour_quantum_number(bottom).

conserved_quantum_number(baryon).
conserved_quantum_number(lepton).
conserved_quantum_number(weak_isospin).
conserved_quantum_number(electric_charge).
conserved_quantum_number(x_charge).

%%	particle(+Particle) is semidet.
%%	particle(-Particle) is multi.
particle(P) :- ground_semidet(P, particle_nd).
particle_nd(P) :- elementary(P).
particle_nd(P) :- combined(P).

%%	proper_anti_particle(+Particle, ?AntiParticle) is semidet.
%%  proper_anti_particle(?Particle, +AntiParticle) is semidet.
%%	proper_anti_particle(?Particle, ?AntiParticle) is nondet.
proper_anti_particle(Particle, anti(Particle)) :-
    ground(Particle),
    atom(Particle),
    !,
    \+ particles:anti_particle_self(Particle),
    particles:particle(Particle).

proper_anti_particle(Particle0, anti(ParticleN)) :-
    particles:particle(Particle0),
    \+ particles:anti_particle_self(Particle0),
    Particle0 \= anti(_),
    (   color(Particle0, Flavour, Color)
    ->  anti_color(Particle1, Flavour, anti(Color)),
        Particle1 = anti(ParticleN)
    ;   ParticleN = Particle0
    ).

particles:spin(AntiParticle, Spin) :-
    proper_anti_particle(Particle, AntiParticle),
    particles:spin(Particle, Spin).


:- begin_tests(particles).

test('proper_anti_particle(proton, anti(proton))') :-
    proper_anti_particle(proton, anti(proton)).

:- end_tests(particles).
