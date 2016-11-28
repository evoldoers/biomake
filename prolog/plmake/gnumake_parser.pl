% * -*- Mode: Prolog -*- */

:- module(gnumake_parser,
          [
              parse_gnu_makefile/2
	  ]).

:- use_module(library(pio)).

% Wrapper for reading GNU Makefile
parse_gnu_makefile(F,M) :-
    debug(makefile,'reading: ~w\n',[F]),
    phrase_from_file(makefile_rules(M,1),F),
    debug(makefile,"rules: ~w\n",[M]).

% Grammar for reading GNU Makefile
makefile_rules([],_) --> call(eos), !.
makefile_rules(Rules,Line) --> comment, !, {Lnext is Line + 1}, makefile_rules(Rules,Lnext).
makefile_rules(Rules,Line) --> blank_line, !, {Lnext is Line + 1}, makefile_rules(Rules,Lnext).
makefile_rules([Rule|Rules],Line) --> makefile_rule(Rule,Lrule), !, {Lnext is Line + Lrule}, makefile_rules(Rules,Lnext).
makefile_rules([Assignment|Rules],Line) --> makefile_assignment(Assignment), !, {Lnext is Line + 1}, makefile_rules(Rules,Lnext).
makefile_rules(_,Line) --> line_as_string(L), !, {format(string(Err),"GNU makefile parse error at line ~d: ~w",[Line,L]),syntax_error(Err)}.

eos([], []).

makefile_assignment(assignment(Var,Val)) -->
    makefile_atom(Var),
    opt_whitespace,
    "=",
    opt_whitespace,
    line_as_string(Val).

makefile_rule(rule(Head,Deps,Exec),Lines) -->
    makefile_targets(Head),
    ":",
    opt_makefile_targets(Deps),
    "\n",
    !,
    makefile_execs(Exec,Lexecs),
    {Lines is 1 + Lexecs}.

opt_makefile_targets(T) --> makefile_targets(T).
opt_makefile_targets([]) --> !.

makefile_targets([T|Ts]) --> opt_whitespace, makefile_token(T), whitespace, makefile_targets(Ts), opt_whitespace.
makefile_targets([T]) --> opt_whitespace, makefile_token(T), opt_whitespace.

makefile_token(T) --> makefile_token_chars(Tc), {string_chars(T,Tc)}.
makefile_atom(T) --> makefile_token_chars(Tc), {atom_chars(T,Tc)}.
			       
makefile_token_chars([C]) --> makefile_token_char(C).
makefile_token_chars([C|Rest]) --> makefile_token_char(C), makefile_token_chars(Rest).

% I think the following is mixing up codes (C\=10) with chars (C\=':')
% ...probably not safe
makefile_token_char(C) --> [C],{C\='$',C\='%',C\=':',C\=' ',C\='\n',C\='\r',C\='\t',C\=10},!.

whitespace --> " ", !, opt_whitespace.
whitespace --> "\t", !, opt_whitespace.

opt_whitespace --> whitespace.
opt_whitespace --> !.

blank_line --> opt_whitespace, "\n", !.

makefile_execs([E|Es],Lines) --> makefile_exec(E), !, {Lines = Lrest + 1}, makefile_execs(Es,Lrest).
makefile_execs(Es,Lines) --> comment, !, {Lines = Lrest + 1}, makefile_execs(Es,Lrest).
makefile_execs([],0) --> !.

makefile_exec(E) --> "\t", !, line(Ec), {string_chars(E,Ec)}.

line([]) --> ( "\n" ; call(eos) ), !.
line([]) --> comment.
line([L|Ls]) --> [L], line(Ls).

line_as_string(L) --> line(Lc), {string_chars(L,Lc)}.

comment --> opt_whitespace, "#", line(_).
