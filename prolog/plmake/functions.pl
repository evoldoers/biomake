% * -*- Mode: Prolog -*- */

:- module(functions,
          [
           makefile_function/3
           ]).

:- use_module(library(plmake/utils)).
:- use_module(library(plmake/plmake)).

makefile_function(Result) --> lb("subst"), str_arg(From), comma, str_arg(To), comma, str_arg(Src), rb, !,
	{ expand_vars(Src,Sx),
	  expand_vars(From,Fx),
	  expand_vars(To,Tx),
	  string_chars(Sx,Sc),
	  string_chars(Fx,Fc),
	  string_chars(Tx,Tc),
	  phrase(subst(Fc,Tc,Rc),Sc),
	  string_chars(Result,Rc) }.

makefile_function(Result) --> lb("word"), num_arg(N), comma, str_arg(A), rb, !,
	{ expand_vars(A,Ax),
          split_spaces(Ax,L),
	  nth_element(N,L,Result) }.

makefile_function(Result) --> lb("wordlist"), num_arg(S), comma, num_arg(E), comma, str_arg(A), rb, !,
	{ expand_vars(A,Ax),
          split_spaces(Ax,L),
	  slice(S,E,L,Sliced),
	  concat_string_list(Sliced,Result," ") }.

makefile_function(Result) --> lb("words"), str_arg(A), rb, !,
	{ expand_vars(A,Ax),
          split_spaces(Ax,L),
	  length(L,Result) }.

makefile_function(Result) --> lb("firstword"), str_arg(A), rb, !,
	{ expand_vars(A,Ax),
	  split_spaces(Ax,L),
          L = [Result|_] }.

makefile_function(Result) --> lb("lastword"), str_arg(A), rb, !,
	{ expand_vars(A,Ax),
          split_spaces(Ax,L),
	  last_element(L,Result) }.

makefile_function("") --> ['('], whitespace, str_arg(S), [')'], !, {format("Warning: unknown function ~w~n",[S])}.
makefile_function("") --> ['('], str_arg(S), whitespace, [')'], !, {format("Warning: unknown function ~w~n",[S])}.

lb(Func) --> ['('], {string_chars(Func,Cs)}, opt_whitespace, Cs, [' '], !.
rb --> opt_whitespace, [')'].

comma --> opt_whitespace, [','].
str_arg(S) --> opt_whitespace, str_arg_outer(S).
str_arg_outer(S) --> ['('], !, str_arg_inner(Si), [')'], {concat_string_list(["(",Si,")"],S)}.
str_arg_outer(S) --> string_from_chars(Start,"(),"), !, str_arg_outer(Rest), {string_concat(Start,Rest,S)}.
str_arg_outer("") --> !.
str_arg_inner(S) --> ['('], !, str_arg_inner(Si), [')'], {concat_string_list(["(",Si,")"],S)}.
str_arg_inner(S) --> string_from_chars(Start,"()"), !, str_arg_inner(Rest), {string_concat(Start,Rest,S)}.
str_arg_inner("") --> !.

num_arg(N) --> opt_whitespace, num_chars(C), {C\=[],number_chars(N,C)}.
num_chars([]) --> [].
num_chars([C|Cs]) --> num_char(C), num_chars(Cs).
num_char(X) --> [X],{X@>='0',X@=<'9'},!.    % foo('0') %

subst(Cs,Ds,Result) --> Cs, !, subst(Cs,Ds,Rest), {append(Ds,Rest,Result)}.
subst(Cs,Ds,[C|Rest]) --> [C], !, subst(Cs,Ds,Rest).
subst(_Cs,_Ds,[]) --> !.
