:- module(particles, [
                      particle/1,
                      anti_particle_self/1,
                      proper_anti_particle/2,
                      elementary/1,
                      combined/1,
                      color_charge/2,
                      spin/2,
                      symbol/2,
                      quantum_number/1,
                      quantum_number/3,
                      conserved_quantum_number/1,
                      flavour_quantum_number/1
                     ]).

:- meta_predicate anti_property(1,+,2,+).
:- multifile anti_particle_self/1.
:- multifile symbol/2.
:- multifile spin/2.
:- multifile color_charge/2.
:- multifile quantum_number/3.
%%	quantum_number(+NumberType, +AntiParticle, ?AntiNumber) is semidet.
particles:quantum_number(NumberType, AntiParticle, AntiNumber) :-
    (   ground(AntiParticle)
    ->  proper_anti_particle(Particle, AntiParticle),
        !
    ;   proper_anti_particle(Particle, AntiParticle)
    ),
    particles:quantum_number(NumberType, Particle, Number),
    AntiNumber is -Number.

:- use_module(utils).
:- use_module(quarks, []).
:- use_module(fermions, [fermion/1]).
:- use_module(bosons, [elementary_boson/1]).
:- use_module(hadrons, [classical_hadron/1]).
:- use_module(color_charge, [color/3, anti_color/3]).

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
%%	particle(?Particle) is nondet.
particle(P) :- ground_semidet(P, particle_nd).
particle_nd(P) :- elementary(P).
particle_nd(P) :- combined(P).

elementary(P) :- elementary_boson(P).
elementary(P) :- fermion(P).

combined(P) :- classical_hadron(P).

%%	proper_anti_particle(+Particle, ?AntiParticle) is semidet.
%%  proper_anti_particle(?Particle, +AntiParticle) is semidet.
%%	proper_anti_particle(?Particle, ?AntiParticle) is nondet.
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

