% * -*- Mode: Prolog -*- */

:- module(gnumake_parser,
          [
              parse_gnu_makefile/4
	  ]).

:- use_module(library(pio)).
:- use_module(library(biomake/utils)).
:- use_module(library(biomake/functions)).
:- use_module(library(biomake/biomake)).

% Wrapper for reading GNU Makefile
parse_gnu_makefile(F,M,OptsOut,OptsIn) :-
	parse_gnu_makefile('',F,M,OptsOut,OptsIn).

parse_gnu_makefile(DirSlash,F,M,OptsOut,OptsIn) :-
    debug(makefile,'reading: ~w~n',[F]),
    atom_string(MAKEFILE_LIST,"MAKEFILE_LIST"),
    Assignment = assignment(MAKEFILE_LIST,"+=",F),
    add_gnumake_clause(Assignment,OptsIn,OptsIn),
    format(string(Path),"~w~w",[DirSlash,F]),
    phrase_from_file(makefile_rules(Mf,OptsOut,OptsIn,1,Path),Path),
    M = [Assignment|Mf],
    debug(makefile,"rules: ~w~noptions: ~w",[M,OptsOut]).

% Grammar for reading GNU Makefile
makefile_rules([],Opts,Opts,_,_) --> call(eos), !.
makefile_rules(Rules,OptsOut,OptsIn,Line,File) -->
	comment, !, {Lnext is Line + 1}, makefile_rules(Rules,OptsOut,OptsIn,Lnext,File).
makefile_rules(Rules,OptsOut,OptsIn,Line,File) -->
	blank_line, !, {Lnext is Line + 1}, makefile_rules(Rules,OptsOut,OptsIn,Lnext,File).
makefile_rules(Rules,OptsOut,OptsIn,Line,File) -->
	info_line, !, {Lnext is Line + 1}, makefile_rules(Rules,OptsOut,OptsIn,Lnext,File).
makefile_rules(Rules,OptsOut,OptsIn,Line,File) -->
	warning_line, !, {Lnext is Line + 1}, makefile_rules(Rules,OptsOut,OptsIn,Lnext,File).
makefile_rules(Rules,OptsOut,OptsIn,Line,File) -->
	error_line, !, {Lnext is Line + 1}, makefile_rules(Rules,OptsOut,OptsIn,Lnext,File).
makefile_rules([option(Opt)|Rules],OptsOut,OptsIn,Line,File) -->
	makefile_special_target(Opt,Lt), !, {Lnext is Line + Lt}, makefile_rules(Rules,OptsOut,[Opt|OptsIn],Lnext,File).
makefile_rules(Rules,OptsOut,OptsIn,Line,File) -->
	include_line(Included,Opts,OptsIn), !, {Lnext is Line + 1, append(Included,Next,Rules)},
	makefile_rules(Next,OptsOut,Opts,Lnext,File).
makefile_rules([Assignment|Rules],OptsOut,OptsIn,Line,File) -->
	makefile_assignment(Assignment,Lass), !, {add_gnumake_clause(Assignment,OptsIn,OptsIn), Lnext is Line + Lass},
	makefile_rules(Rules,OptsOut,OptsIn,Lnext,File).
makefile_rules([Rule|Rules],OptsOut,OptsIn,Line,File) -->
	makefile_rule(Rule,Lrule), !, {add_gnumake_clause(Rule,OptsIn,OptsIn), Lnext is Line + Lrule},
	makefile_rules(Rules,OptsOut,OptsIn,Lnext,File).
makefile_rules(_,_,_,Line,File) -->
	opt_space, "\t", !,
	{format(string(Err),"GNU makefile parse error at line ~d of file ~w: unexpected tab character",[Line,File]),
	syntax_error(Err)}.
makefile_rules(_,_,_,Line,File) -->
	line_as_string(L), !,
	{format(string(Err),"GNU makefile parse error at line ~d of file ~w: ~w",[Line,File,L]),
	syntax_error(Err)}.

eos([], []).

error_line -->
    opt_space,
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
    opt_space,
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
    opt_space,
    "$(info",
    whitespace,
    makefile_warning_text(W),
    ")",
    opt_whitespace,
    "\n",
    !,
    {format("~w~n",[W])}.

include_line(Rules,OptsOut,OptsIn) -->
    opt_space,
    "include",
    whitespace,
    include_makefiles(Rules,OptsOut,OptsIn).

include_makefiles(Rules,OptsOut,OptsIn) -->
	makefile_filename_string(F), opt_whitespace, "\n", !,
	{include_gnu_makefile(F,Rules,OptsOut,OptsIn)}.
include_makefiles(Rules,OptsOut,OptsIn) -->
	makefile_filename_string(F), whitespace, !,
	{include_gnu_makefile(F,R,Opts,OptsIn)},
	include_makefiles(Next,OptsOut,Opts),
	{append(R,Next,Rules)}.

include_gnu_makefile(F,R,Opts,OptsIn) :-
	(bagof(Dslash,
	       (member(include_dir(D),OptsIn),
	        format(atom(Dslash),"~w/",[D])),
	       RevDirs)
	 ; RevDirs = []),
	reverse(RevDirs,Dirs),
	search_include_dirs(F,['','./'|Dirs],R,Opts,OptsIn).

search_include_dirs(F,[],_,_,_) :-
	format(string(Err),"Couldn't find included makefile ~w~n",[F]),
	throw(Err).
search_include_dirs(F,[Dir|_],R,Opts,OptsIn) :-
	format(string(Path),"~w/~w",[Dir,F]),
	exists_file(Path),
	format("Found ~w~n",Path),
	!,
	parse_gnu_makefile(Dir,F,R,Opts,OptsIn).
search_include_dirs(F,[_|Dirs],R,Opts,OptsIn) :-
	search_include_dirs(F,Dirs,R,Opts,OptsIn).

makefile_assignment(assignment(Var,Op,Val),Lines) -->
    opt_space,
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
    opt_space,
    "define",
    whitespace,
    makefile_var_atom_from_codes(Var),
    opt_whitespace,
    "\n",
    makefile_def_body(Cs,BodyLines),
    {string_chars(Val,Cs),
     Lines is BodyLines + 1}.

makefile_assignment(assignment(Var,Op,Val),1) -->
    opt_space,
    makefile_var_atom_from_codes(Var),
    opt_whitespace,
    op_string(Op),
    opt_whitespace,
    line_as_string(Val).

makefile_special_target(queue(none),Lines) -->
    makefile_rule(rule([".NOTPARALLEL"],_,_),Lines).

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
opt_makefile_targets([]) --> opt_space.

makefile_targets([T|Ts]) --> opt_space, makefile_target_string(T), whitespace, makefile_targets(Ts), opt_whitespace.
makefile_targets([T]) --> opt_space, makefile_target_string(T), opt_whitespace.

makefile_warning_text(S) --> string_from_codes(S,")").
makefile_filename_string(S) --> string_from_codes(S," \t\n").
makefile_target_string(S) --> string_from_codes(S,":; \t\n").

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

makefile_def_body([],1) --> opt_space, "endef", !, opt_whitespace, "\n".
makefile_def_body(['\n'|Cs],Lplus1) --> ['\n'], !, makefile_def_body(Cs,L), {Lplus1 is L + 1}.
makefile_def_body([C|Cs],Lines) --> [C], makefile_def_body(Cs,Lines).

comment --> opt_space, "#", line(_).
