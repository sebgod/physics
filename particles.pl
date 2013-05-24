:- module(particles, [
                      particle/1,
                      anti_particle_self/1,
                      not_anti_particle/1,
                      proper_anti_particle/2,
                      elementary/1,
                      combined/1
                     ]).

:- meta_predicate anti_property(1,+,2,+).
:- multifile anti_particle_self/1.
:- multifile elementary/1.
:- multifile combined/1.

:- use_module(utils, [call_semidet_ground/2 as particle_call_semidet]).
:- use_module(color_charge, [color/3, anti_color/3]).

:- meta_predicate particle_call_semidet(1,?).

%%	not_anti_particle(+Particle) is semidet.
not_anti_particle(Particle) :- Particle \= anti(_).

%%	particle(+Particle) is semidet.
%%	particle(-Particle) is multi.
particle(P) :- particle_call_semidet(particle_nd, P).
particle_nd(P) :- elementary(P).
particle_nd(P) :- combined(P).

%%	proper_anti_particle(+Particle, ?AntiParticle) is semidet.
%%  proper_anti_particle(?Particle, +AntiParticle) is semidet.
%%	proper_anti_particle(?Particle, ?AntiParticle) is nondet.
proper_anti_particle(Particle0, anti(ParticleN)) :-
    ground(ParticleN),
    atom(ParticleN),
    !,
    \+ particles:anti_particle_self(ParticleN),
    Particle0 = ParticleN,
    particles:particle(Particle0).

proper_anti_particle(Particle0, anti(ParticleN)) :-
    particles:particle(Particle0),
    \+ particles:anti_particle_self(Particle0),
    Particle0 \= anti(_),
    (   color(Particle0, Flavour, Color)
    ->  anti_color(Particle1, Flavour, anti(Color)),
        Particle1 = anti(ParticleN)
    ;   ParticleN = Particle0
    ).

:- begin_tests(particles).

test('proper_anti_particle(proton, anti(proton))') :-
    proper_anti_particle(proton, anti(proton)).

:- end_tests(particles).
