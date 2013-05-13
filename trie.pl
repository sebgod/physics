:- module(trie, [
                 empty/1,
                 is_trie/1,
                 list_to_trie/2,
                 add/3,
                 contains/2,
                 print_trie/1
                ]).

:- use_module(library(assoc)).
:- multifile user:portray/1.

user:portray(trie(A, L)) :-
    is_assoc(A),
    is_list(L),
    print_trie(trie(A, L)).

print_space(0) :- !.
print_space(L) :-
    L1 is L-1,
    write(' '),
    print_space(L1).

print_trie(T) :- print_trie(T, 0).
print_trie(trie(A, L), Level) :-
    print(L),
    nl,
    Level1 is Level + 1,
    forall(gen_assoc(Key, A, Value),
          (   print_space(Level),
              format('~w-', [Key]),
              print_trie(Value, Level1)
          )
          ).

%%	empty(-Trie) is det.
%%	empty(?Trie) is semidet.
empty(trie(E, [])) :- empty_assoc(E).

is_trie(E) :- empty(E).
is_trie(trie(A, L)) :-
    is_list(L),
    is_assoc(A).

list_to_trie(Atoms, Trie) :-
    empty(Empty),
    foldl(add, Atoms, Empty, Trie).

%%	add(+Atom, +Trie0, -TrieN) is det.
add(Atom, T0, TN) :- add(Atom, Atom, T0, TN).

add(Atom, '', trie(Assoc, List0), trie(Assoc, ListN)) :-
    sort([Atom | List0], ListN), !.

add(Atom, LeftChars0, trie(Assoc, List0), trie(Assoc, ListN)) :-
    LeftChars0 \= '',
    Atom \= LeftChars0,
    length(List0, ListLength),
    ListLength =< 3,
    empty_assoc(Assoc),
    sort([Atom | List0], ListN), !.

add(Atom, LeftChars0, trie(Assoc0, List), trie(AssocN, List)) :-
    LeftChars0 \= '',
    prefix(Atom, LeftChars0, Prefix, LeftCharsN),
    (   get_assoc(Prefix,
                  Assoc0, Sub0,
                  AssocN, SubN
                 ),
        !
    ;   empty(Sub0),
        put_assoc(Prefix, Assoc0, SubN, AssocN)
    ),
    add(Atom, LeftCharsN, Sub0, SubN).

prefix(Atom, LeftChars0, Prefix, LeftCharsN) :-
    atom_length(Atom, AL),
    atom_length(LeftChars0, LCL),
    PrefixLength is max(1, min(round(sqrt(AL)), LCL)),
    sub_atom(LeftChars0, 0, PrefixLength, After, Prefix),
    sub_atom(LeftChars0, PrefixLength, After, 0, LeftCharsN).

contains(Trie, Atom) :-
    contains(Trie, Atom, Atom).

contains(Trie, Atom, CharsLeft) :-
    (   atom(Atom)
    ->  contains_semidet(Trie, Atom, CharsLeft)
    ;   contains_nd(Trie, Atom, CharsLeft)
    ).
contains_semidet(trie(Assoc, List), Atom, LeftChars0) :-
    (   memberchk(Atom, List)
    ;   prefix(Atom, LeftChars0, Prefix, LeftCharsN),
        get_assoc(Prefix, Assoc, Sub),
        contains_semidet(Sub, Atom, LeftCharsN)
    ).
contains_nd(trie(_Assoc, List), Atom, _CharsLeft) :-
    (   member(Atom, List)
    ;   false
    ).

remove(Atom, trie(Assoc0, List0), trie(AssocN, ListN)) :-
   (   ord_memberchk(Atom, List0)
   ->  ord_del_element(List0, Atom, ListN),
       AssocN = Assoc0
   ;   false
   ).

:- begin_tests(trie).

test(add_to_empty,
     [setup(empty(T0))]) :- add(hallo, T0, _TN).

test(list_to_trie,
    [setup( (empty(T0), add(hallo, T0, TN) ) )]
    ) :-
    list_to_trie([hallo], TN).

test(add_if_exists,
     [setup(list_to_trie([hallo], T0)),
      TN == T0
     ]) :-
    add(hallo, T0, TN).

test(add_many, [setup(Items = [abc, abcd, ab123, ab456, ab4567])]) :-
    list_to_trie(Items, T),
    forall(member(Item, Items), assertion(contains(T, Item))).

:- end_tests(trie).
