:- module(symbols, [symbol/2, symbol_mf/2]).

:- multifile symbol_mf/2.

symbol(Thing, Symbol) :- symbol_mf(Thing, Symbol).
