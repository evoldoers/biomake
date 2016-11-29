% * -*- Mode: Prolog -*- */

:- module(gnumake_parser,
          [
              parse_gnu_makefile/2
	  ]).

:- use_module(library(pio)).

% Wrapper for reading GNU Makefile
parse_gnu_makefile(F,M) :-
    debug(makefile,'reading: ~w~n',[F]),
    phrase_from_file(makefile_rules(Mf,1,F),F),
    atom_string(MAKEFILE_LIST,"MAKEFILE_LIST"),
    M = [assignment(MAKEFILE_LIST,"+=",F)|Mf],
    debug(makefile,"rules: ~w\n",[M]).

% Grammar for reading GNU Makefile
makefile_rules([],_,_) --> call(eos), !.
makefile_rules(Rules,Line,File) --> comment, !, {Lnext is Line + 1}, makefile_rules(Rules,Lnext,File).
makefile_rules(Rules,Line,File) --> blank_line, !, {Lnext is Line + 1}, makefile_rules(Rules,Lnext,File).
makefile_rules(Rules,Line,File) --> info_line, !, {Lnext is Line + 1}, makefile_rules(Rules,Lnext,File).
makefile_rules(Rules,Line,File) --> warning_line, !, {Lnext is Line + 1}, makefile_rules(Rules,Lnext,File).
makefile_rules(Rules,Line,File) --> error_line, !, {Lnext is Line + 1}, makefile_rules(Rules,Lnext,File).
makefile_rules(Rules,Line,File) --> include_line(Included), !, {Lnext is Line + 1, append(Included,Next,Rules)}, makefile_rules(Next,Lnext,File).
makefile_rules([Assignment|Rules],Line,File) --> makefile_assignment(Assignment), !, {Lnext is Line + 1}, makefile_rules(Rules,Lnext,File).
makefile_rules([Rule|Rules],Line,File) --> makefile_rule(Rule,Lrule), !, {Lnext is Line + Lrule}, makefile_rules(Rules,Lnext,File).
makefile_rules(_,Line,File) --> line_as_string(L), !, {format(string(Err),"GNU makefile parse error at line ~d of file ~w: ~w",[Line,File,L]),syntax_error(Err)}.

eos([], []).

error_line -->
    opt_whitespace,
    "$(error",
    whitespace,
    makefile_warning_text(W),
    ")",
    opt_whitespace,
    "\n",
    !,
    {format(string(Warning),"~w~n",[W]),
     write(user_error,Warning),
     throw(Warning)}.

warning_line -->
    opt_whitespace,
    "$(warning",
    whitespace,
    makefile_warning_text(W),
    ")",
    opt_whitespace,
    "\n",
    !,
    {format(string(Warning),"~w~n",[W]),
     write(user_error,Warning)}.

info_line -->
    opt_whitespace,
    "$(info",
    whitespace,
    makefile_warning_text(W),
    ")",
    opt_whitespace,
    "\n",
    !,
    {format("~w~n",[W])}.

include_line(Rules) -->
    opt_whitespace,
    "include",
    whitespace,
    include_makefiles(Rules).

include_makefiles(Rules) --> makefile_filename_string(F), opt_whitespace, "\n", !, {parse_gnu_makefile(F,Rules)}.
include_makefiles(Rules) --> makefile_filename_string(F), whitespace, !, {parse_gnu_makefile(F,R)}, include_makefiles(Next), {append(R,Next,Rules)}.

makefile_assignment(assignment(Var,Op,Val)) -->
    opt_whitespace,
    makefile_var_atom(Var),
    opt_whitespace,
    op_string(Op),
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

makefile_targets([T|Ts]) --> opt_whitespace, makefile_target_string(T), whitespace, makefile_targets(Ts), opt_whitespace.
makefile_targets([T]) --> opt_whitespace, makefile_target_string(T), opt_whitespace.

makefile_warning_text(S) --> {string_codes(")",XL)}, string_toks(S,XL).
makefile_filename_string(S) --> {string_codes(" \t\n",XL)}, string_toks(S,XL).
makefile_target_string(S) --> {string_codes(": \t\n",XL)}, string_toks(S,XL).
makefile_var_atom(S) --> {string_codes(":?+= \t\n",XL)}, atom_toks(S,XL).

op_string("=") --> "=".
op_string(":=") --> ":=".
op_string("?=") --> "?=".
op_string("+=") --> "+=".

string_toks(S,XL) --> clist(C,XL), {C\=[], string_chars(S,C)}.
atom_toks(S,XL) --> clist(C,XL), {C\=[], atom_chars(S,C)}.

clist([C|Cs],XL) --> [C], {forall(member(X,XL),C\=X)}, !, clist(Cs,XL).
clist([C|Cs],XL) --> ['\\'], [C], !, clist(Cs,XL).
clist([],_) --> [].

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
