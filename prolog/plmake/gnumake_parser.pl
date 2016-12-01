% * -*- Mode: Prolog -*- */

:- module(gnumake_parser,
          [
              parse_gnu_makefile/2,
	      makefile_var_char/3,
	      makefile_var_chars/3,
	      makefile_var_atom_from_chars/3,
	      makefile_var_string_from_chars/3
	  ]).

:- use_module(library(pio)).
:- use_module(library(plmake/utils)).

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
makefile_rules([Assignment|Rules],Line,File) --> makefile_assignment(Assignment,Lass), !, {Lnext is Line + Lass}, makefile_rules(Rules,Lnext,File).
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

makefile_assignment(assignment(Var,Op,Val),Lines) -->
    opt_whitespace,
    "define",
    whitespace,
    makefile_var_atom_from_codes(Var),
    opt_whitespace,
    op_string(Op),
    opt_whitespace,
    "\n",
    makefile_def_body(Cs,BodyLines),
    {string_chars(Val,Cs),
     Lines is BodyLines + 1}.

makefile_assignment(assignment(Var,"=",Val),Lines) -->
    opt_whitespace,
    "define",
    whitespace,
    makefile_var_atom_from_codes(Var),
    opt_whitespace,
    "\n",
    makefile_def_body(Cs,BodyLines),
    {string_chars(Val,Cs),
     Lines is BodyLines + 1}.

makefile_assignment(assignment(Var,Op,Val),1) -->
    opt_whitespace,
    makefile_var_atom_from_codes(Var),
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

makefile_rule(rule(Head,Deps,[Efirst|Erest]),Lines) -->
    makefile_targets(Head),
    ":",
    opt_makefile_targets(Deps),
    ";",
    line_as_string(Efirst),
    "\n",
    !,
    makefile_execs(Erest,Lexecs),
    {Lines is 1 + Lexecs}.

opt_makefile_targets(T) --> makefile_targets(T).
opt_makefile_targets([]) --> opt_whitespace.

makefile_targets([T|Ts]) --> opt_whitespace, makefile_target_string(T), whitespace, makefile_targets(Ts), opt_whitespace.
makefile_targets([T]) --> opt_whitespace, makefile_target_string(T), opt_whitespace.

makefile_warning_text(S) --> string_from_codes(S,")").
makefile_filename_string(S) --> string_from_codes(S," \t\n").
makefile_target_string(S) --> string_from_codes(S,":; \t\n").

% We allow only a restricted subset of characters in variable names,
% compared to the GNU make specification.
% (seriously, does anyone use makefile variable names containing brackets, commas, colons, etc?)
makefile_var_char(C) --> alphanum_char(C).
makefile_var_char('_') --> ['_'].
makefile_var_char('-') --> ['-'].

makefile_var_chars([]) --> [].
makefile_var_chars([C|Cs]) --> makefile_var_char(C), makefile_var_chars(Cs).

makefile_var_atom_from_chars(A) --> makefile_var_chars(Cs), {atom_chars(A,Cs)}.
makefile_var_string_from_chars(S) --> makefile_var_chars(Cs), {string_chars(S,Cs)}.

% define these again as character codes, because Prolog is so annoying
makefile_var_code(C) --> alphanum_code(C).
makefile_var_code(95) --> [95].  % underscore '_'
makefile_var_code(45) --> [45].  % hyphen '-'

makefile_var_codes([]) --> [].
makefile_var_codes([C|Cs]) --> makefile_var_code(C), makefile_var_codes(Cs).

makefile_var_atom_from_codes(A) --> makefile_var_codes(Cs), {atom_codes(A,Cs)}.
makefile_var_string_from_codes(S) --> makefile_var_codes(Cs), {string_codes(S,Cs)}.


op_string("=") --> "=".
op_string(":=") --> ":=".
op_string("?=") --> "?=".
op_string("+=") --> "+=".
op_string("!=") --> "!=".

makefile_execs([E|Es],Lines) --> makefile_exec(E), !, {Lines = Lrest + 1}, makefile_execs(Es,Lrest).
makefile_execs(Es,Lines) --> comment, !, {Lines = Lrest + 1}, makefile_execs(Es,Lrest).
makefile_execs([],0) --> !.

makefile_exec(E) --> "\t", !, line_as_string(E).

line([]) --> ( "\n" ; call(eos) ), !.
line([]) --> comment.
line([L|Ls]) --> [L], line(Ls).

line_as_string(S) --> line(Sc), {string_chars(S,Sc)}.

makefile_def_body([],1) --> opt_whitespace, "endef", !, opt_whitespace, "\n".
makefile_def_body(['\n'|Cs],Lplus1) --> ['\n'], !, makefile_def_body(Cs,L), {Lplus1 is L + 1}.
makefile_def_body([C|Cs],Lines) --> [C], makefile_def_body(Cs,Lines).

comment --> opt_whitespace, "#", line(_).
