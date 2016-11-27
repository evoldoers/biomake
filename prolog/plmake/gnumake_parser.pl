% * -*- Mode: Prolog -*- */

:- module(gnumake_parser,
          [
              parse_gnu_makefile/2
	  ]).

:- use_module(library(pio)).

% Wrapper for reading GNU Makefile
parse_gnu_makefile(F,M) :-
    debug(makefile,'reading: ~w\n',[F]),
    phrase_from_file(makefile_rules(M),F),
    debug(makefile,"rules: ~w\n",[M]).

% Grammar for reading GNU Makefile
makefile_rules([]) --> call(eos), !.
makefile_rules([Rule|Rules]) --> makefile_rule(Rule), makefile_rules(Rules).

eos([], []).

makefile_rule(rule(Head,Deps,Exec)) -->
    comments_or_blanks,
    makefile_targets(Head),
    ":",
    makefile_targets(Deps),
    "\n",
    !,
    makefile_execs(Exec),
    comments_or_blanks.

makefile_targets([T|Ts]) --> opt_whitespace, makefile_target_chars(Tc), {string_chars(T,Tc)}, whitespace, makefile_targets(Ts), opt_whitespace.
makefile_targets([T]) --> opt_whitespace, makefile_target_chars(Tc), {string_chars(T,Tc)}, opt_whitespace.

makefile_target_chars([C]) --> makefile_target_char(C).
makefile_target_chars([C|Rest]) --> makefile_target_char(C), makefile_target_chars(Rest).

whitespace --> " ", !, opt_whitespace.
whitespace --> "\t", !, opt_whitespace.

opt_whitespace --> whitespace.
opt_whitespace --> !.

blank_line --> opt_whitespace, "\n", !.

makefile_target_char(C) --> [C],{C\='$',C\='%',C\=':',C\=' ',C\='\n',C\='\r',C\='\t',C\=10},!.

makefile_execs([E|Es]) --> makefile_exec(E), !, makefile_execs(Es).
makefile_execs(Es) --> comment, makefile_execs(Es).
makefile_execs([]) --> !.

makefile_exec(E) --> "\t", !, line(Ec), {string_chars(E,Ec)}.

line([]) --> ( "\n" ; call(eos) ), !.
line([]) --> comment.
line([L|Ls]) --> [L], line(Ls).

comment --> opt_whitespace, "#", line(_).

comments_or_blanks --> comment, !, comments_or_blanks.
comments_or_blanks --> blank_line, !, comments_or_blanks.
comments_or_blanks --> [].
