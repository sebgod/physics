:- module(color_charge, [
                         primary_color/1,
                         color/1,
                         anti_color/1,
                         white/3,
                         color/3,
                         primary_color/3,
                         anti_color/3,
                         gluon_color/1
                        ]).

:- use_module(library(lists)).

primary_color(red).
primary_color(green).
primary_color(blue).

color(C) :- primary_color(C).
color(anti(C)) :- anti_color(anti(C)).

anti_color(anti(C)) :- primary_color(C).

white(anti(C1), anti(C2), anti(C3)) :-
    permutation([C1, C2, C3], [red, green, blue]).
white(C1, C2, C3) :-
    permutation([C1, C2, C3], [red, green, blue]).

color(Particle, Flavour, Color) :-
    color(Particle, Flavour, Color, 1).

primary_color(Particle, Flavour, Color) :-
    primary_color(Particle, Flavour, Color, 1).
anti_color(AntiParticle, Flavour, AntiColor) :-
    anti_color(AntiParticle, Flavour, AntiColor, 1).

color(Particle, Flavour, Color, Args) :-
    primary_color(Particle, Flavour, Color, Args).
color(anti(Particle), Flavour, Color, Args) :-
    anti_color(anti(Particle), Flavour, Color, Args).

primary_color(Particle, Flavour, Color, Args) :-
    (   ground(Flavour) -> Flavour \= anti(_) ; true),
    functor(Particle, Flavour, Args),
    arg(1, Particle, Color),
    primary_color(Color).

anti_color(anti(Particle), Flavour, AntiColor, Args) :-
    functor(Particle, Flavour, Args),
    arg(1, Particle, AntiColor),
    anti_color(AntiColor).


gluon_color((red*anti(blue) + blue*anti(red))**(1 rdiv 2)).
gluon_color(-i * (red*anti(blue) - blue*anti(red))**(1 rdiv 2)).

gluon_color((red*anti(green) + green*anti(red))**(1 rdiv 2)).
gluon_color(-i * (red*anti(green) - green*anti(red))**(1 rdiv 2)).

gluon_color((blue*anti(green) + green*anti(blue))**(1 rdiv 2)).
gluon_color(-i * (blue*anti(green) - green*anti(blue))**(1 rdiv 2)).

gluon_color((red*anti(red) + blue*anti(blue))**(1 rdiv 2)).
gluon_color((red*anti(red) + green*anti(green) - 2*blue*anti(blue))**(1 rdiv 6)).
