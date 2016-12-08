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
	makefile_block(BlockRules,BlockOptsOut,OptsIn,Line,File,BlockLines),
	!, { Lnext is Line + BlockLines, append(BlockRules,NextRules,Rules)},
	makefile_rules(NextRules,OptsOut,BlockOptsOut,Lnext,File).

eos([], []).

makefile_block([],Opts,Opts,_,_,1) --> comment, !.
makefile_block([],Opts,Opts,_,_,1) --> blank_line, !.
makefile_block([],Opts,Opts,_,_,1) --> info_line, !.
makefile_block([],Opts,Opts,_,_,1) --> warning_line, !.
makefile_block([],Opts,Opts,_,_,1) --> error_line, !.
makefile_block([option(Opt)],[Opt|Opts],Opts,_,_,Lines) --> makefile_special_target(Opt,Lines), !.
makefile_block(Rules,OptsOut,OptsIn,Line,File,Lines) --> makefile_conditional(Rules,OptsOut,OptsIn,Line,File,Lines), !.
makefile_block(Rules,OptsOut,OptsIn,_,_,1) --> include_line(Rules,OptsOut,OptsIn), !.
makefile_block([Assignment],Opts,Opts,_,_,Lines) --> makefile_assignment(Assignment,Lines), !,
	{add_gnumake_clause(Assignment,Opts,Opts)}.
makefile_block([Rule],Opts,Opts,_,_,Lines) --> makefile_recipe(Rule,Lines), !,
	{add_gnumake_clause(Rule,Opts,Opts)}.
makefile_block(_,_,_,Line,File,_) -->
	opt_space, "\t", !,
	{format(string(Err),"GNU makefile parse error at line ~d of file ~w: unexpected tab character",[Line,File]),
	syntax_error(Err)}.
makefile_block(_,_,_,Line,File,_) -->
	line_as_string(L), !,
	{format(string(Err),"GNU makefile parse error at line ~d of file ~w: ~w",[Line,File,L]),
	syntax_error(Err)}.

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

makefile_conditional(Result,OptsOut,OptsIn,Line,File,Lines) -->
    opt_space, "ifeq", whitespace, conditional_arg_pair(Arg1,Arg2), opt_whitespace, "\n",
    !, {Arg1 = Arg2 -> Status = true; Status = false},
    eval_true_false_rules(Status,Result,OptsOut,OptsIn,Line,File,Lines).

makefile_conditional(Result,OptsOut,OptsIn,Line,File,Lines) -->
    opt_space, "ifneq", whitespace, conditional_arg_pair(Arg1,Arg2), opt_whitespace, "\n",
    !, {Arg1 \= Arg2 -> Status = true; Status = false},
    eval_true_false_rules(Status,Result,OptsOut,OptsIn,Line,File,Lines).

makefile_conditional(Result,OptsOut,OptsIn,Line,File,Lines) -->
    opt_space, "ifdef", whitespace, xvar(Arg), opt_whitespace, "\n",
    !, {Arg \= "" -> Status = true; Status = false},
    eval_true_false_rules(Status,Result,OptsOut,OptsIn,Line,File,Lines).

makefile_conditional(Result,OptsOut,OptsIn,Line,File,Lines) -->
    opt_space, "ifndef", whitespace, xvar(Arg), opt_whitespace, "\n",
    !, {Arg = "" -> Status = true; Status = false},
    eval_true_false_rules(Status,Result,OptsOut,OptsIn,Line,File,Lines).

conditional_arg_pair(Arg1,Arg2) --> "(", xbracket(Arg1), ",", xbracket(Arg2), ")".
conditional_arg_pair(Arg1,Arg2) --> "'", xquote(Arg1), ",", xquote(Arg2), "'".
conditional_arg_pair(Arg1,Arg2) --> "\"", xdblquote(Arg1), ",", xdblquote(Arg2), "\"".

eval_true_false_rules(true,TrueResult,TrueOptsOut,OptsIn,Line,File,Lines) -->
    { Lnext is Line + 1 },
    true_false_rules(TrueResult,_FalseResult,TrueOptsOut,OptsIn,_FalseOptsOut,OptsIn,Lnext,File,Ltf),
    { Lines is Ltf + 1 }.

eval_true_false_rules(false,FalseResult,FalseOptsOut,OptsIn,Line,File,Lines) -->
    { Lnext is Line + 1 },
    true_false_rules(_TrueResult,FalseResult,_TrueOptsOut,OptsIn,FalseOptsOut,OptsIn,Lnext,File,Ltf),
    { Lines is Ltf + 1 }.

true_false_rules([],[],TrueOptsIn,TrueOptsIn,FalseOptsIn,FalseOptsIn,_,_,1) -->
    opt_space, "endif", !, opt_whitespace, "\n".

true_false_rules([],FalseRules,TrueOptsIn,TrueOptsIn,FalseOptsOut,FalseOptsIn,Line,File,Lines) -->
    opt_space, "else", !, opt_whitespace, "\n",
    { Lnext is Line + 1 },
    false_rules(FalseRules,FalseOptsOut,FalseOptsIn,Lnext,File,FalseLines),
    { Lines is FalseLines + 1}.

true_false_rules(TrueRules,FalseRules,TrueOptsOut,TrueOptsIn,FalseOptsOut,FalseOptsIn,Line,File,Lines) -->
    makefile_block(BlockRules,BlockOptsOut,TrueOptsIn,Line,File,BlockLines),
    !, { Lnext is Line + BlockLines, append(BlockRules,NextRules,TrueRules) },
    true_false_rules(NextRules,FalseRules,TrueOptsOut,BlockOptsOut,FalseOptsOut,FalseOptsIn,Lnext,File,NextLines),
    { Lines is BlockLines + NextLines }.

true_false_rules(_,_,_,_,_,_,Line,File,_) -->
    line_as_string(L), !,
    {format(string(Err),"GNU makefile parse error (expected else/endif) at line ~d of file ~w: ~w",[Line,File,L]),
    syntax_error(Err)}.

false_rules([],OptsIn,OptsIn,_,_,1) -->
    opt_space, "endif", !, opt_whitespace, "\n".

false_rules(Rules,OptsOut,OptsIn,Line,File,Lines) -->
    makefile_block(BlockRules,BlockOptsOut,OptsIn,Line,File,BlockLines),
    !, { Lnext is Line + BlockLines, append(BlockRules,NextRules,Rules) },
    false_rules(NextRules,OptsOut,BlockOptsOut,Lnext,File,NextLines),
    { Lines is BlockLines + NextLines }.

false_rules(_,_,_,Line,File,_) -->
    line_as_string(L), !,
    {format(string(Err),"GNU makefile parse error (expected endif) at line ~d of file ~w: ~w",[Line,File,L]),
    syntax_error(Err)}.

xbracket(Sx) --> {char_code('(',L),char_code(')',R)}, xdelim(Sx,L,R).
xquote(Sx) --> {char_code('\'',Q)}, xdelim(Sx,Q,Q).
xdblquote(Sx) --> {char_code('"',Q)}, xdelim(Sx,Q,Q).
xvar(Sx) --> makefile_var_string_from_codes(S), {eval_var(S,Sx,v(null,null,null,[]))}.
xdelim(Sx,L,R) --> delim(S,L,R), !, {expand_vars(S,Sx,v(null,null,null,[]))}.
delim(S,L,R) --> opt_whitespace, delim_outer(Sc,L,R), {string_codes(S,Sc)}.
delim_outer(S,L,R) --> [L], !, delim_inner(I,L,R), [R], delim_outer(Rest,L,R),
	{ append([L|I],[R],LIR), append(LIR,Rest,S) }.
delim_outer(S,L,R) --> {char_code(',',C)}, code_list(Start,[L,R,C]), !, delim_outer(Rest,L,R), {append(Start,Rest,S)}.
delim_outer([],_,_) --> !.
delim_inner(S,L,R) --> [L], !, delim_inner(I,L,R), [R], {append([L|I],[R],S)}.
delim_inner(S,L,R) --> code_list(Start,[L,R]), !, delim_inner(Rest,L,R), {append(Start,Rest,S)}.
delim_inner([],_,_) --> !.
    

makefile_special_target(queue(none),Lines) -->
    makefile_recipe(rule([".NOTPARALLEL"],_,_),Lines).

makefile_recipe(rule(Head,Deps,Exec),Lines) -->
    makefile_targets(Head),
    ":",
    opt_makefile_targets(Deps),
    "\n",
    !,
    makefile_execs(Exec,Lexecs),
    {Lines is 1 + Lexecs}.

makefile_recipe(rule(Head,Deps,[Efirst|Erest]),Lines) -->
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
