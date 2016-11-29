% * -*- Mode: Prolog -*- */

:- module(functions,
          [
           makefile_function/3
           ]).

:- use_module(library(plmake/plmake)).

makefile_function(Result) --> lb("firstword"), str_arg(A), rb, !,
	{ expand_vars(A,Ax),
	  split_spaces(Ax,L),
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

makefile_function(Result) --> lb("wordlist"), num_arg(S), comma, num_arg(E), comma, str_arg(A), rb, !,
	{ expand_vars(A,Ax),
          split_spaces(Ax,L),
	  slice(S,E,L,Sliced),
	  concat_string_list(Sliced,Result," ") }.

makefile_function("") --> ['('], whitespace, str_arg(S), [')'], !, {format("Warning: unknown function ~w~n",[S])}.
makefile_function("") --> ['('], str_arg(S), whitespace, [')'], !, {format("Warning: unknown function ~w~n",[S])}.

last_element([],"").
last_element([X],X).
last_element([_|Ls],X) :- last_element(Ls,X).

nth_element(_,[],"").
nth_element(1,[X|_],X).
nth_element(N,[_|Ls],X) :- Np is N - 1, nth_element(Np,Ls,X).

slice(_S,_E,[],[]).
slice(1,E,[L|Ls],[L|Rs]) :- E > 0, En is E - 1, slice(1,En,Ls,Rs).
slice(S,E,[_L|Ls],R) :- Sn is S - 1, En is E - 1, slice(Sn,En,Ls,R).

lb(Func) --> ['('], {string_chars(Func,Cs)}, opt_whitespace, Cs, [' '], !.
rb --> opt_whitespace, [')'].

comma --> opt_whitespace, [','].
str_arg(S) --> opt_whitespace, str_arg_inner(S).
str_arg_inner(S) --> ['('], !, str_arg_inner(Si), [')'], {concat_string_list(["(",Si,")"],S)}.
str_arg_inner(S) --> string_toks(Start,"()"), !, str_arg_inner(Rest), {string_concat(Start,Rest,S)}.
str_arg_inner("") --> !.

string_toks(S,XS) --> {string_chars(XS,XL)}, clist(C,XL), {C\=[], string_chars(S,C)}.
atom_toks(S,XS) --> {string_chars(XS,XL)}, clist(C,XL), {C\=[], atom_chars(S,C)}.

clist([C|Cs],XL) --> [C], {forall(member(X,XL),C\=X)}, !, clist(Cs,XL).
clist([C|Cs],XL) --> ['\\'], [C], !, clist(Cs,XL).
clist([],_) --> [].

num_arg(N) --> opt_whitespace, num_chars(C), {C\=[],number_chars(N,C)}.
num_chars([]) --> [].
num_chars([C|Cs]) --> num_char(C), num_chars(Cs).
num_char(X) --> [X],{X@>='0',X@=<'9'},!.    % foo('0') %

concat_string_list(L,S) :- concat_string_list(L,S,"").
concat_string_list([],"",_).
concat_string_list([S],S,_).
concat_string_list([L|Ls],F,Sep) :- concat_string_list(Ls,R,Sep), string_concat(L,Sep,Lsep), string_concat(Lsep,R,F).

split_spaces(S,L) :-
	split_string(S," "," ",L).

whitespace --> [' '], !, opt_whitespace.
whitespace --> ['\t'], !, opt_whitespace.

opt_whitespace --> whitespace.
opt_whitespace --> !.
