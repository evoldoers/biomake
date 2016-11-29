% * -*- Mode: Prolog -*- */

:- module(functions,
          [
           makefile_function/3
           ]).

:- use_module(library(plmake/plmake)).

makefile_function(Result) --> lb("firstword"), str_arg(A), rb, !,
	{ debug(function,"$(firstword ~w)", [A]),
	  !,
	  expand_vars(A,Ax),
	  !,
          debug(function,"Ax=~w", [Ax]),
	  split_spaces(Ax,L),
          debug(function,"L=~w", [L]),
	  L = [Result|_] }.

makefile_function(Result) --> lb("lastword"), str_arg(A), rb, !,
	{ expand_vars(A,Ax),
          split_spaces(Ax,L),
	  last_element(L,Result) }.

makefile_function(Result) --> lb("words"), str_arg(A), rb, !,
	{ expand_vars(A,Ax),
          split_spaces(Ax,L),
	  length(L,Result) }.

makefile_function(Result) --> lb("word"), num_arg(N), comma, str_arg(A), rb, !,
	{ expand_vars(A,Ax),
          split_spaces(Ax,L),
	  nth_element(N,L,Result) }.

last_element([],"").
last_element([X],X).
last_element([_|Ls],X) :- last_element(Ls,X).

nth_element(_,[],"").
nth_element(1,[X|_],X).
nth_element(N,[_|Ls],X) :- Np is N - 1, nth_element(Np,Ls,X).


lb(Func) --> ['$','('], {string_chars(Func,Cs)}, opt_whitespace, Cs, [' '], !.
rb --> opt_whitespace, [')'].

comma --> opt_whitespace, [','].
str_arg(S) --> opt_whitespace, string_toks(S,")").

string_toks(S,XS) --> {string_chars(XS,XL)}, clist(C,XL), {C\=[], string_chars(S,C)}.
atom_toks(S,XS) --> {string_chars(XS,XL)}, clist(C,XL), {C\=[], atom_chars(S,C)}.

clist([C|Cs],XL) --> [C], {forall(member(X,XL),C\=X)}, !, clist(Cs,XL).
clist([C|Cs],XL) --> ['\\'], [C], !, clist(Cs,XL).
clist([],_) --> [].

num_arg(N) --> opt_whitespace, num_chars(C), {C\=[],number_chars(N,C)}.
num_chars([]) --> [].
num_chars([C|Cs]) --> num_char(C), num_chars(Cs).
num_char(X) --> [X],{X@>='0',X@=<'9'},!.    % foo('0') %

split_spaces(S,L) :-
	split_string(S," "," ",L).

whitespace --> [' '], !, opt_whitespace.
whitespace --> ['\t'], !, opt_whitespace.

opt_whitespace --> whitespace.
opt_whitespace --> !.
