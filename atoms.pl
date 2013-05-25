:- module(atoms, [
                  atom/5,
                  normalize/2,
                  atomic_number/2,
                  isotope/3,
                  noble/1,
                  noble_shell/2,
                  noble_gas_below/3,
                  atom_block/2,
                  block/3,
                  atom_orbitals/2,
                  electron_configuration/1,
                  electron_configuration/2,
                  electron_configuration_pairs/3
                 ]).

:- use_module(particle_taxonomy).
:- use_module(symbols, [symbol/2]).
:- use_module(quantum_numbers, []).
:- use_module(utils, [
                      findnsols/4 as find_electron_configs,
                      between2d/4 as in_block,
                      call_semidet_ground/2 as atoms_call_semidet,
                      call_semidet_ground/3 as block_call_semidet,
                      call_semidet_ground_first/3 as atoms_call_semidet_first
                     ]).
:- use_module(library(apply), [foldl/4 as reduce_noble_shell_foldl]).

:- meta_predicate
    atoms_call_semidet(1, ?),
    block_call_semidet(2, ?, ?),
    find_electron_configs(+, ?, :, -),
    atoms_call_semidet_first(2, ?, ?),
    reduce_noble_shell_foldl(3, +, +, -).

write_symbol(Obj) :- once(symbol(Obj, Symbol)), write(Symbol).

user:portray(shell(N, L, C)) :-
    N > 0, L >= 0, C > 0,
    write_symbol(shell(N, L, C)).

user:portray(orbital(N, L, ML, S)) :-
    N > 0, L >= 0, integer(ML), rational(S),
    write_symbol(orbital(N, L, ML, S)).

user:portray(atoms:Atom) :-
    atom(Atom),
    write_symbol(atoms:Atom).

symbols:symbol_mf(sharp, s).
symbols:symbol_mf(principal, p).
symbols:symbol_mf(diffuse, d).
symbols:symbol_mf(fundamental, f).

atom(h,   1, hydrogen,    1, 1).
atom(he,  2, helium,     18, 1).
atom(li,  3, lithium,     1, 2).
atom(be,  4, beryllium,   2, 2).
atom(b,   5, boron,      13, 2).
atom(c,   6, carbon,     14, 2).
atom(n,   7, nitrogen,   15, 2).
atom(o,   8, oxygen,     16, 2).
atom(f,   9, flurine,    17, 2).
atom(ne, 10, neon,       18, 2).
atom(na, 11, sodium,      1, 3).
atom(mg, 12, magnesium,   2, 3).
atom(al, 13, aluminium,  13, 3).
atom(si, 14, silicon,    14, 3).
atom(p,  15, phosphorus, 15, 3).
atom(s,  15, sulfur,     16, 3).
atom(cl, 17, chlorine,   17, 3).
atom(ar, 18, argon,      18, 3).
atom(k,  19, potassium,   1, 4).
atom(ca, 20, calcium,     2, 4).
atom(sc, 21, scandium,    3, 4).
atom(ti, 22, titanium,    4, 4).
atom(v,  23, vanadium,    5, 4).
atom(cr, 24, chromium,    6, 4).
atom(mn, 25, manganese,   7, 4).
atom(fe, 26, iron,        8, 4).
atom(co, 27, cobalt,      9, 4).
atom(ni, 28, nickel,     10, 4).
atom(cu, 29, copper,     11, 4).
atom(zn, 30, zinc,       12, 4).
atom(ga, 31, gallium,    13, 4).
atom(ge, 32, germanium,  14, 4).
atom(as, 33, arsenic,    15, 4).
atom(se, 34, selenium,   16, 4).
atom(br, 35, bromine,    17, 4).
atom(kr, 36, krypton,    18, 4).
atom(rb, 37, ribidium,    1, 5).
atom(sr, 38, strontium,   2, 5).
atom(y,  39, yttrium,     3, 5).
atom(zr, 40, zirconium,   4, 5).
atom(nb, 41, niobium,     5, 5).
atom(mo, 42, molybdenum,  6, 5).
atom(tc, 43, technetium,  7, 5).
atom(ru, 44, ruthenium,   8, 5).
atom(rh, 45, rhodium,     9, 5).
atom(pd, 46, palladium,  10, 5).
atom(ag, 47, silver,     11, 5).
atom(cd, 48, cadmium,    12, 5).
atom(in, 49, indium,     13, 5).
atom(sn, 50, tin,        14, 5).
atom(sb, 51, antimony,   15, 5).
atom(te, 52, tellerium,  16, 5).
atom(i,  53, iodine,     17, 5).
atom(xe, 54, xenon,      18, 5).
atom(cs, 55, caesium,     1, 6).
atom(ba, 56, barium,      2, 6).
atom(la, 57, lanthanum,   l, 6).
atom(ce, 58, cerium,      l, 6).
atom(pr, 59, praseodymium,l, 6).
atom(nd, 60, neodymium,   l, 6).
atom(pm, 61, promethium,  l, 6).
atom(sm, 62, samarium,    l, 6).
atom(eu, 63, europium,    l, 6).
atom(gd, 64, gadolinium,  l, 6).
atom(tb, 65, terbium,     l, 6).
atom(dy, 66, dysprosium,  l, 6).
atom(ho, 67, holmium,     l, 6).
atom(er, 68, erbium,      l, 6).
atom(tm, 69, thulium,     l, 6).
atom(yb, 70, ytterbium,   l, 6).
atom(lu, 71, lutetium,    l(lu), 6).
atom(hf, 72, hafnium,     4, 7).
atom(ta, 73, tantalum,    5, 6).
atom(w,  74, tungsten,    6, 6).
atom(re, 75, rhenium,     7, 6).
atom(os, 76, osmium,      8, 6).
atom(ir, 77, iridium,     9, 6).
atom(pt, 78, platinum,   10, 6).
atom(au, 79, gold,       11, 6).
atom(hg, 80, mercury,    12, 6).
atom(tl, 81, thallium,   13, 6).
atom(pb, 82, lead,       14, 6).
atom(bi, 83, bismuth,    15, 6).
atom(po, 84, polonium,   16, 6).
atom(at, 85, astatine,   17, 6).
atom(rn, 86, radon,      18, 6).
atom(fr, 87, francium,    1, 7).
atom(ra, 88, radium,      2, 7).
atom(ac, 89, actinium,    a, 7).
atom(th, 90, thorium,     a, 7).
atom(pa, 91, protactinium,a, 7).
atom(u,  92, uranium,     a, 7).
atom(np, 93, neptunium,   a, 7).
atom(pu, 94, plutonium,   a, 7).
atom(am, 95, americium,   a, 7).
atom(cm, 96, curium,      a, 7).
atom(bk, 97, berkelium,   a, 7).
atom(cf, 98, californium, a, 7).
atom(es, 99, einsteinium, a, 7).
atom(fm,100, fermium,     a, 7).
atom(md,101, mendelevium, a, 7).
atom(no,102, nobelium,    a, 7).
atom(lr,103, lawrencium,  a(lr), 7).
atom(rf,104, rutherfordium,4, 7).
atom(db,105, dubnium,     5, 7).
atom(sg,106, seaborgium,  6, 7).
atom(bh,107, bohrium,     7, 7).
atom(hs,108, hassium,     8, 7).
atom(mt,109, meitnerium,  9, 7).
atom(ds,110, darmstadtium,10, 7).
atom(rg,111, roentgenium, 11, 7).
atom(cn,112, copernicium, 12, 7).
atom(uut,113,ununtrium,   13, 7).
atom(fl,114, flerovium,   14, 7).
atom(uup,115,ununpentium, 15, 7).
atom(lv,116, livermorium, 16, 7).
atom(uus,117,ununseptium, 17, 7).
atom(uuo,118,ununoctium,  18, 7).

normalize(atoms:Atom, atoms:Atom) :- atom(Atom), !, atom(Atom, _, _, _, _).

normalize(Atom0, atoms:Atom) :-
    atom(Atom0),
    !,
    atom_length(Atom0, Length),
    (   Length =< 3 -> atom(Atom0, _, _, _, _),
        Atom = Atom0
    ;    atom(Atom, _, Atom0, _, _)
    ).
normalize(Atom, atoms:Atom) :-
    var(Atom),
    atom(Atom, _, _, _, _).

isotope(h, protium, 0).
isotope(h, deuterium, 1).
isotope(h, tritium, 2).

atom_block(Atom, Block) :-
    atom(Atom, _, _, G, P),
    block(P, G, Block).

%%	block(+Period, +Group, -Block) is semidet.
%%	block(?Period, ?Group, -Block) is nondet.
block(P, G, B) :- block_call_semidet(block_nd(B), P, G).

%%	block_nd(?Block, ?Period, ?Group) is nondet.
block_nd(1-s, 1, 1).
block_nd(1-s, 1, 18).
block_nd(4-f, 4, l).
block_nd(5-f, 5, a).
block_nd(5-d, 6, l(lu)).
block_nd(6-d, 7, a(lr)).
block_nd(P-s, P, G) :- in_block(2-7, 1-2, P, G).
block_nd(P-p, P, G) :- in_block(2-7, 13-18, P, G).
block_nd(P1-d, P, G) :-
    in_block(3-7, 3-13, P, G),
    utils:safe_is(P1, P - 1).

noble(Atom) :- normalize(Atom, Normalized), noble_(Normalized).
noble_(atoms:Atom) :- atoms_call_semidet(noble_nd, Atom).

noble_nd(he).
noble_nd(ne).
noble_nd(ar).
noble_nd(kr).
noble_nd(xe).
noble_nd(rn).

atomic_number(Atom, AtomicNumber) :-
    atomic_number(Atom, AtomicNumber, _).
atomic_number(Atom, AtomicNumber, atoms:Symbol) :-
    normalize(Atom, atoms:Symbol),
    atom(Symbol, AtomicNumber, _, _, _).

electron_configuration_pairs(Electrons, Configs, Pairs) :-
    electron_configuration(Electrons, Configs),
    setof(P-L, ML^S^member(orbital(P, L, ML, S), Configs), Pairs).

atom_orbitals(Atom, AggrOrbitals) :-
    (   var(Atom) -> atomic_number(_, AtomicNumber, Atom)
    ;   atomic_number(Atom, AtomicNumber)
    ),
    Electrons = AtomicNumber,
    electron_configuration_pairs(Electrons, Configs, Pairs),
    maplist(atom_orbitals_count(Configs), Pairs, Shells),
    (   Electrons > 2
    ->  reduce_noble_shells(Electrons, Shells, AggrOrbitals)
    ;   AggrOrbitals = Shells
    ).

atom_orbitals_count(Orbitals, P-L, shell(P, L, Count)) :-
    aggregate_all(count, member(orbital(P, L, _, _), Orbitals), Count).

principal_number(Principal) :-
    principal_number(inf, Principal).
principal_number(Max, Principal) :-
    between(1, Max, Principal).

azimuthal_number(Max, Azimuthal) :-
    between(0, Max, Azimuthal).

principal_azimuthal(Principal, Azimuthal) :-
    Principal >= 1,
    Max is Principal - 1,
    azimuthal_number(Max, Azimuthal).
azimuthal_magnetic(Azimuthal, Magnetic) :-
    utils:safe_is(LV, Azimuthal),
    Min is -LV,
    Max is +LV,
    between(Min, Max, Magnetic).

electron_configuration(Electrons, Orbitals) :-
    find_electron_configs(Electrons, O,
                          electron_configuration(O), Orbitals).

electron_configuration(orbital(P, L, M, S)) :-
    principal_number(PL),
    principal_number(PL, P),
    azimuthal_number(PL, L),
    PL =:= P + L,
    principal_azimuthal(P, L),
    (   S is +1 rdiv 2
    ;   S is -1 rdiv 2
    ),
    azimuthal_magnetic(L, M).

reduce_noble_shells(Electrons, Orbitals, AggrOrbitals) :-
    findall(Noble, noble_gas_below(Electrons, Noble, _), Nobles),
    reduce_noble_shell_foldl(reduce_noble_shell,
                             Nobles, Orbitals, AggrOrbitals).

reduce_noble_shell(Noble, Orbitals, AggrOrbitals) :-
    (   noble_shell(Noble, NobleShell)
    ->  append(NobleShell, OuterShell, Orbitals),
        append([Noble], OuterShell, AggrOrbitals)
    ;   AggrOrbitals = Orbitals
    ).

noble_shell(Noble, Shell) :-
    atoms_call_semidet_first(noble_shell_nd, Noble, Shell).

:- dynamic noble_shell_dyn/2.
noble_shell_nd(Noble, NobleShell) :-
    (   noble_shell_dyn(Noble, NobleShell)
    ;   atom_orbitals(Noble, NobleShell),
        atom(Noble),
        noble(Noble),
        assert(noble_shell_dyn(Noble, NobleShell))
    ).

noble_gas_below(E, Noble, Rest) :-
    E > 1,
    noble(Noble),
    atom(Noble, NobleNumber, _, _, _),
    E > NobleNumber,
    utils:safe_is(Rest, E rem NobleNumber).

quantum_numbers:quantum_number_mf(principal, p=P, P) :-
    principal_number(P).

quantum_numbers:quantum_number_mf(azimuthal, n=Principal, Azimuthal) :-
    principal_azimuthal(Principal, Azimuthal).

quantum_numbers:quantum_number_mf(magnetic, l=L, Ml) :-
    azimuthal_magnetic(L, Ml).

symbols:symbol_mf(atoms:Atom, Symbol) :-
    atomic_number(atoms:Atom, AtomicNumber),
    utils:term_sub(AtomicNumber, AtomicNumberSub),
    format(atom(Symbol), '~w~w', [AtomicNumberSub, Atom]).

symbols:symbol_mf(shell(P, L, Count), Symbol) :-
    azimuthal_symbol(L, LS),
    utils:term_sup(Count, CountSup),
    format(atom(Symbol), '~d~w~w', [P, LS, CountSup]).

symbols:symbol_mf(orbital(P, L, ML, S), Symbol) :-
    azimuthal_symbol(L, LS),
    utils:term_sup(ML, MLSup),
    (   utils:safe_is(S, +1 rdiv 2) -> SS = '↑'
    ;   utils:safe_is(S, -1 rdiv 2) -> SS = '↓'
    ),
    format(atom(Symbol), '~d~w~w~w', [P, LS, MLSup, SS]).

azimuthal_symbol(Number, Char) :-
    (   var(Char), Number >= 7
    ->  Code is 0'k + Number - 7, atom_codes(Char, Code)
    ;   Number = 0 -> Char = s
    ;   Number = 1 -> Char = p
    ;   Number = 2 -> Char = d
    ;   Number = 3 -> Char = f
    ;   Number = 4 -> Char = g
    ;   Number = 5 -> Char = h
    ;   Number = 6 -> Char = i
    ;   (
         var(Number),
         atom(Char),
         atom_length(Char, 1),
         atom_codes(Char, Code)
        )
    ->  Number is Code - 0'k,
        Number >= 7
    ).

:- begin_tests(atoms).

test('normalize(he)', N == atoms:he) :-
    normalize(he, N).

test('atom_block(he)', B == 1-s) :- atom_block(he, B).

test('electron_spin', ElectronSpin = 1 rdiv 2) :-
    once(quantum_numbers:quantum_number(spin, electron, ElectronSpin)).

test('atomic_number(h, 1)') :- atomic_number(atoms:h, 1).
test('atomic_number(carbon, 1)') :- atomic_number(carbon, 6).

:- end_tests(atoms).



