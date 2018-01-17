% * -*- Mode: Prolog -*- */

:- module(gnumake_parser,
          [
              parse_gnu_makefile/4,
	      eval_gnu_makefile/4
	  ]).

:- use_module(library(pio)).
:- use_module(library(biomake/utils)).
:- use_module(library(biomake/functions)).
:- use_module(library(biomake/biomake)).

% Declare all debug topics defined in this module
:- nodebug(makefile).

% Wrapper for reading GNU Makefile
parse_gnu_makefile(F,M,OptsOut,OptsIn) :-
    parse_gnu_makefile('',F,M,OptsOut,OptsIn).

parse_gnu_makefile(DirSlash,F,M,OptsOut,OptsIn) :-
    debug(makefile,'reading: ~w',[F]),

    atom_string(MAKEFILE_LIST,"MAKEFILE_LIST"),
    MakefileListAssignment = assignment(MAKEFILE_LIST,"+=",F),
    add_gnumake_clause(MakefileListAssignment,OptsIn,OptsIn),

    (bagof(G,member(toplevel(G),OptsIn),MakeCmdGoals)
     ; MakeCmdGoals = []),
    atomic_list_concat(MakeCmdGoals,' ',MakeCmdGoalStr),
    atom_string(MAKECMDGOALS,"MAKECMDGOALS"),
    MakeCmdGoalsAssignment = assignment(MAKECMDGOALS,"=",MakeCmdGoalStr),
    add_gnumake_clause(MakeCmdGoalsAssignment,OptsIn,OptsIn),

    format(string(Path),"~w~w",[DirSlash,F]),
    phrase_from_file(makefile_rules(Mf,OptsOut,OptsIn,1,Path),Path),
    M = [MakefileListAssignment,MakeCmdGoalsAssignment|Mf],
    debug(makefile,"rules: ~w~noptions: ~w",[M,OptsOut]).

eval_gnu_makefile(Text,M,OptsOut,OptsIn) :-
    debug(makefile,'evaluating: ~w',[Text]),
    string_codes(Text,Codes),
    phrase(makefile_rules(M,OptsOut,OptsIn,1,"(eval)"),Codes),
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
makefile_block([],Opts,Opts,_,_,Lines) --> info_line(Lines), !.
makefile_block([],Opts,Opts,Line,File,Lines) --> warning_line(Line,File,Lines), !.
makefile_block([],Opts,Opts,Line,File,_) --> error_line(Line,File), !.
makefile_block(Rules,OptsOut,OptsIn,Line,File,Lines) --> prolog_block(true,Rules,OptsOut,OptsIn,Line,File,Lines).
makefile_block(Rules,OptsOut,OptsIn,Line,File,Lines) --> makefile_conditional(true,Rules,OptsOut,OptsIn,Line,File,Lines), !.
makefile_block(Rules,OptsOut,OptsIn,_,File,1) --> include_line(true,File,Rules,OptsOut,OptsIn), !.
makefile_block([Assignment],Opts,Opts,_,_,Lines) --> makefile_assignment(Assignment,Lines), !,
	{add_gnumake_clause(Assignment,Opts,Opts)}.
makefile_block([option(Opt)],[Opt|Opts],Opts,_,_,Lines) --> makefile_special_target(Opt,Lines), !.
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

ignore_makefile_block(Opts,Opts,Line,File,Lines) --> prolog_block(false,_,_,Opts,Line,File,Lines).
ignore_makefile_block(Opts,Line,File,Lines) --> makefile_conditional(false,_,_,Opts,Line,File,Lines), !.
ignore_makefile_block(Opts,_,_,1) --> include_line(false,null,_,Opts,Opts), !.
ignore_makefile_block(_Opts,_,_,Lines) --> makefile_assignment(_,Lines), !.
ignore_makefile_block(_Opts,_,_,Lines) --> makefile_special_target(_,Lines), !.
ignore_makefile_block(_Opts,_,_,Lines) --> makefile_recipe(_,Lines), !.
ignore_makefile_block(Opts,Line,File,Lines) --> makefile_block([],Opts,Opts,Line,File,Lines).

prolog_block(Active,Rules,OptsOut,OptsIn,Line,File,Lines) -->
    opt_space,
    "prolog",
    opt_period,
    opt_whitespace,
    "\n",
    { Lnext is Line + 1 },
    prolog_block_body(RawLines,Lnext,File,Lbody),
    { Lines is Lbody + 1,
      read_prolog_from_string(Active,Rules,OptsOut,OptsIn,RawLines) },
    !.

prolog_block_body(_,_,File,_) -->
    call(eos),
    { format(string(Err),"GNU makefile parse error (expected endprolog) at end of file ~w",[File]),
      syntax_error(Err) }.

prolog_block_body([],_,_,1) -->
    opt_space,
    "endprolog",
    opt_period,
    opt_whitespace,
    "\n",
    !.

prolog_block_body([RawLine|RawLines],Line,File,Lines) -->
    line_as_string(RawLine,1),
    { Lnext is Line + 1 },
    prolog_block_body(RawLines,Lnext,File,Lbody),
    { Lines is Lbody + 1 },
    !.

opt_period --> ".".
opt_period --> [].

read_prolog_from_string(false,[],Opts,Opts,_).
read_prolog_from_string(true,Rules,OptsOut,OptsIn,RawLines) :-
    concat_string_list(RawLines,Raw,"\n"),
    open_string(Raw,IOS),
    read_makeprog_stream(IOS,OptsOut,OptsIn,Terms),
    maplist(wrap_prolog,Terms,Rules).

wrap_prolog(Term,prolog(Term)).

error_line(Line,File) -->
    opt_space,
    "$(error",
    whitespace,
    makefile_warning_text(W,_),
    ")",
    opt_whitespace,
    "\n",
    !,
    {format(string(Warning),"~w:~w: ~w~n",[File,Line,W]),
     write(user_error,Warning),
     throw(Warning)}.

warning_line(Line,File,Lines) -->
    opt_space,
    "$(warning",
    whitespace,
    makefile_warning_text(W,NL),
    ")",
    opt_whitespace,
    "\n",
    !,
    {format(string(Warning),"~w:~w: ~w~n",[File,Line,W]),
     write(user_error,Warning),
     Lines is NL + 1}.

info_line(Lines) -->
    opt_space,
    "$(info",
    whitespace,
    makefile_warning_text(W,NL),
    ")",
    opt_whitespace,
    "\n",
    !,
    {format("~w~n",[W]),
     Lines is NL + 1}.

include_line(Active,CurrentFile,Rules,OptsOut,OptsIn) -->
    opt_space,
    "include",
    whitespace,
    include_makefiles(Active,CurrentFile,Rules,OptsOut,OptsIn).

include_makefiles(Active,CurrentFile,Rules,OptsOut,OptsIn) -->
	makefile_filename_string(F), opt_whitespace, "\n", !,
	{Active -> include_gnu_makefile(F,CurrentFile,Rules,OptsOut,OptsIn) ; true}.
include_makefiles(Active,CurrentFile,Rules,OptsOut,OptsIn) -->
	makefile_filename_string(F), whitespace, !,
	{Active -> include_gnu_makefile(F,CurrentFile,R,Opts,OptsIn) ; true},
	include_makefiles(Next,CurrentFile,OptsOut,Opts),
	{append(R,Next,Rules)}.

include_gnu_makefile(F,CurrentFile,R,Opts,OptsIn) :-
        expand_vars(F,XF),
	(bagof(Dslash,
	       (member(include_dir(D),OptsIn),
	        format(atom(Dslash),"~w/",[D])),
	       RevDirs)
	 ; RevDirs = []),
	reverse(RevDirs,Dirs),
	file_directory_name(CurrentFile,CurrentFileDir),
	format(atom(CurrentFileDirSlash),"~w/",[CurrentFileDir]),
	search_include_dirs(XF,CurrentFile,['','./',CurrentFileDirSlash|Dirs],R,Opts,OptsIn).

search_include_dirs(F,CurrentFile,[],_,_,_) :-
	format(string(Err),"Couldn't find makefile ~w included from ~w",[F,CurrentFile]),
	throw(Err).
search_include_dirs(F,_,[Dir|_],R,Opts,OptsIn) :-
	format(string(Path),"~w/~w",[Dir,F]),
	exists_file(Path),
	!,
	parse_gnu_makefile(Dir,F,R,Opts,OptsIn).
search_include_dirs(F,CurrentFile,[_|Dirs],R,Opts,OptsIn) :-
	search_include_dirs(F,CurrentFile,Dirs,R,Opts,OptsIn).

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
    {string_codes(Val,Cs),
     Lines is BodyLines + 1}.

makefile_assignment(assignment(Var,"=",Val),Lines) -->
    opt_space,
    "define",
    whitespace,
    makefile_var_atom_from_codes(Var),
    opt_whitespace,
    "\n",
    makefile_def_body(Cs,BodyLines),
    {string_codes(Val,Cs),
     Lines is BodyLines + 1}.

makefile_assignment(assignment(Var,Op,Val),Lines) -->
    opt_space,
    makefile_var_atom_from_codes(Var),
    opt_whitespace,
    op_string(Op),
    opt_whitespace,
    line_as_string(Val,Lines).

makefile_conditional(Active,Result,OptsOut,OptsIn,Line,File,Lines) -->
    opt_space, "ifeq", whitespace, conditional_arg_pair(Active,Arg1,Arg2), opt_whitespace, "\n",
    !, {test_equal(Active,Arg1,Arg2,Condition)},
    begin_true_rules(Condition,Result,OptsOut,OptsIn,Line,File,Lines).

makefile_conditional(Active,Result,OptsOut,OptsIn,Line,File,Lines) -->
    opt_space, "ifneq", whitespace, conditional_arg_pair(Active,Arg1,Arg2), opt_whitespace, "\n",
    !, {test_inequal(Active,Arg1,Arg2,Condition)},
    begin_true_rules(Condition,Result,OptsOut,OptsIn,Line,File,Lines).

makefile_conditional(Active,Result,OptsOut,OptsIn,Line,File,Lines) -->
    opt_space, "ifdef", whitespace, axvar(Active,Arg),
    !, {test_inequal(Active,Arg,'',Condition)},
    begin_true_rules(Condition,Result,OptsOut,OptsIn,Line,File,Lines).

makefile_conditional(Active,Result,OptsOut,OptsIn,Line,File,Lines) -->
    opt_space, "ifndef", whitespace, axvar(Active,Arg),
    !, {test_equal(Active,Arg,'',Condition)},
    begin_true_rules(Condition,Result,OptsOut,OptsIn,Line,File,Lines).

test_equal(false,_,_,null).
test_equal(true,X,X,true) :- !.
test_equal(true,_,_,false).

test_inequal(false,_,_,null).
test_inequal(true,X,X,false) :- !.
test_inequal(true,_,_,true).

conditional_arg_pair(true,Arg1,Arg2) --> "(", xbracket(Arg1), ",",  opt_whitespace, xbracket(Arg2), ")".
conditional_arg_pair(true,Arg1,Arg2) --> "'", xquote(Arg1), "'", whitespace, "'", xquote(Arg2), "'".
conditional_arg_pair(true,Arg1,Arg2) --> "\"", xdblquote(Arg1), "\"", whitespace, "\"", xdblquote(Arg2), "\"".
conditional_arg_pair(false,_,_) --> "(", null_bracket, ",",  opt_whitespace, null_bracket, ")".
conditional_arg_pair(false,_,_) --> "'", null_quote, "'", whitespace, "'", null_quote, "'".
conditional_arg_pair(false,_,_) --> "\"", null_dblquote, "\"", whitespace, "\"", null_dblquote, "\"".

begin_true_rules(Condition,Rules,OptsOut,OptsIn,Line,File,Lines) -->
    { Lnext is Line + 1 },
    true_rules(Condition,Rules,OptsOut,OptsIn,Lnext,File,Ltf),
    { Lines is Ltf + 1 }.

true_rules(_,[],OptsIn,OptsIn,_,_,1) -->
    opt_space, "endif", !, opt_whitespace, "\n".

true_rules(Condition,Rules,OptsOut,OptsIn,Line,File,Lines) -->
    opt_space, "else", !, opt_whitespace, "\n",
    { Lnext is Line + 1 },
    false_rules(Condition,Rules,OptsOut,OptsIn,Lnext,File,FalseLines),
    { Lines is FalseLines + 1}.

true_rules(true,Rules,OptsOut,OptsIn,Line,File,Lines) -->
    makefile_block(BlockRules,BlockOptsOut,OptsIn,Line,File,BlockLines),
    !, { Lnext is Line + BlockLines, append(BlockRules,NextRules,Rules) },
    true_rules(true,NextRules,OptsOut,BlockOptsOut,Lnext,File,NextLines),
    { Lines is BlockLines + NextLines }.

true_rules(Condition,Rules,OptsOut,OptsIn,Line,File,Lines) -->
    { Condition \= true },
    ignore_makefile_block(OptsIn,Line,File,BlockLines),
    !, { Lnext is Line + BlockLines },
    true_rules(Condition,Rules,OptsOut,OptsIn,Lnext,File,NextLines),
    { Lines is BlockLines + NextLines }.

true_rules(_,_,_,_,Line,File,_) -->
    line_as_string(L), !,
    {format(string(Err),"GNU makefile parse error (expected else/endif) at line ~d of file ~w: ~w",[Line,File,L]),
    syntax_error(Err)}.

false_rules(_,[],OptsIn,OptsIn,_,_,1) -->
    opt_space, "endif", !, opt_whitespace, "\n".

false_rules(false,Rules,OptsOut,OptsIn,Line,File,Lines) -->
    makefile_block(BlockRules,BlockOptsOut,OptsIn,Line,File,BlockLines),
    !, { Lnext is Line + BlockLines, append(BlockRules,NextRules,Rules) },
    false_rules(false,NextRules,OptsOut,BlockOptsOut,Lnext,File,NextLines),
    { Lines is BlockLines + NextLines }.

false_rules(Condition,[],OptsIn,OptsIn,Line,File,Lines) -->
    { Condition \= false },
    ignore_makefile_block(OptsIn,Line,File,BlockLines),
    !, { Lnext is Line + BlockLines },
    false_rules(Condition,[],OptsIn,OptsIn,Lnext,File,NextLines),
    { Lines is BlockLines + NextLines }.

false_rules(_,_,_,_,Line,File,_) -->
    line_as_string(L), !,
    {format(string(Err),"GNU makefile parse error (expected endif) at line ~d of file ~w: ~w",[Line,File,L]),
    syntax_error(Err)}.

xbracket(Sx) --> xdelim(Sx,[[0'(,0')],[0'{,0'}]],[0'),0',],[0'\\,0'\n],0).
null_bracket --> delim(_,[[0'(,0')],[0'{,0'}]],[0'),0',],[0'\\,0'\n],0).

xbrace(Sx,NL) --> xdelim(Sx,[[0'{,0'}]],[0'}],[],NL).
xdelim(Sx,LR,XO,XI,NL) --> delim(S,LR,XO,XI,NL), !, {expand_vars(S,Sx)}.
delim(S,LR,X,XA,NL) --> {bagof(L,member([L,_],LR),XL), append(XL,XA,XI), append(X,XI,XO)}, delim_codes(Sc,LR,XO,XI,NL), {string_codes(S,Sc)}.

% delim_codes(-S,+LR,+XO,+XI,-NL)
% general parser
% S = list of character codes
% LR = list of pairs of delimiter character codes [Left,Right]
% XO = list of character codes that are to be excluded in the outermost context
% XI = list of character codes that are to be excluded in the innermost context
% NL = number of lines matched
delim_codes([0'\s|S],LR,XO,XI,NL) --> [0'\\,0'\n], !, delim_codes(S,LR,XO,XI,NLnext), {NL is NLnext + 1}.
delim_codes([0'\n|S],LR,XO,XI,NL) --> {NL \= 0}, [0'\n], !, delim_codes(S,LR,XO,XI,NLnext), {NL is NLnext + 1}.
delim_codes(S,LR,XO,XI,NL) --> {member([L,R],LR)}, [L], !, delim_codes(I,LR,[R|XI],XI,NLi), [R], delim_codes(Rest,LR,XO,XI,NLo),
       { append([L|I],[R],LIR), append(LIR,Rest,S), NL is NLi + NLo }.
delim_codes([C|Cs],LR,XO,XI,NL) --> [0'\\,C], {member(C,XO)}, !, delim_codes(Cs,LR,XO,XI,NL).
delim_codes([C|Cs],LR,XO,XI,NL) --> [C], {\+ member(C,XO)}, !, delim_codes(Cs,LR,XO,XI,NL).
delim_codes([],_,_,_,0) --> !.

xquote(Sx) --> code_list(C,[0'\']), {string_codes(S,C), expand_vars(S,Sx)}.
null_quote --> code_list(_,[0'\']).
xdblquote(Sx) --> code_list(C,[0'\"]), {string_codes(S,C), expand_vars(S,Sx)}.
null_dblquote --> code_list(_,[0'\"]).
xvar(Sx) --> makefile_var_string_from_codes(S), opt_whitespace, "\n", {eval_var(S,Sx)}.

axvar(true,Sx) --> xvar(Sx).
axvar(false,_) --> makefile_var_string_from_codes(_), opt_whitespace, "\n".

makefile_special_target(queue(none),Lines) -->
    makefile_recipe(rule([".NOTPARALLEL"],_,_),Lines).

makefile_special_target(oneshell(true),Lines) -->
    makefile_recipe(rule([".ONESHELL"],_,_),Lines).

makefile_special_target(phony_targets(TL),Lines) -->
    makefile_special_deplist(".PHONY",TL,Lines).

makefile_special_target(silent_targets(TL),Lines) -->
    makefile_special_deplist(".SILENT",TL,Lines).

makefile_special_target(Opt,Lines) -->
    makefile_special_deplist(".IGNORE",TL,Lines),
    { TL = []
      -> Opt = keep_going_on_error(true)
      ; Opt = ignore_errors_in_targets(TL) }.

makefile_special_deplist(SpecialTarget,DepList,Lines) -->
    makefile_recipe(rule([SpecialTarget],DL,_),Lines),
    {maplist(expand_vars,DL,XDL1),
     maplist(split_spaces,XDL1,XDL2),
     flatten_trim(XDL2,DepList)}.

makefile_recipe(rule(Head,Deps,Exec,{HeadGoal},{DepGoal},VNs),Lines) -->
    makefile_targets(Head),
    whitespace_or_linebreak,
    "{",
    xbrace(HeadGoalAtom,Lhead),
    "}",
    opt_whitespace,
    ":",
    opt_makefile_deps(Deps),
    whitespace_or_linebreak,
    "{",
    xbrace(DepGoalAtom,Ldep),
    "}",
    opt_comment,
    !,
    makefile_execs(Exec,Lexecs),
    { Lines is 1 + Lexecs + Lhead + Ldep,
      read_atom_as_makeprog_term(HeadGoalAtom,HeadGoal,HeadVNs),
      read_atom_as_makeprog_term(DepGoalAtom,DepGoal,DepVNs),
      merge_unifications(HeadVNs,DepVNs,VNs) }.

makefile_recipe(rule(Head,Deps,Exec,{HeadGoal},{true},VNs),Lines) -->
    makefile_targets(Head),
    whitespace_or_linebreak,
    "{",
    xbrace(HeadGoalAtom,Lhead),
    "}",
    opt_whitespace,
    ":",
    opt_makefile_deps(Deps),
    opt_comment,
    !,
    makefile_execs(Exec,Lexecs),
    { Lines is 1 + Lexecs + Lhead,
      read_atom_as_makeprog_term(HeadGoalAtom,HeadGoal,VNs) }.

makefile_recipe(rule(Head,Deps,Exec,{DepGoal},VNs),Lines) -->
    makefile_targets(Head),
    ":",
    opt_makefile_deps(Deps),
    whitespace_or_linebreak,
    "{",
    xbrace(DepGoalAtom,Ldep),
    "}",
    opt_comment,
    !,
    makefile_execs(Exec,Lexecs),
    { Lines is 1 + Lexecs + Ldep,
      read_atom_as_makeprog_term(DepGoalAtom,DepGoal,VNs) }.

makefile_recipe(rule(Head,Deps,Exec),Lines) -->
    makefile_targets(Head),
    ":",
    opt_makefile_deps(Deps),
    opt_comment,
    !,
    makefile_execs(Exec,Lexecs),
    {Lines is 1 + Lexecs}.

makefile_recipe(rule(Head,Deps,[Efirst|Erest]),Lines) -->
    makefile_targets(Head),
    ":",
    opt_makefile_deps(Deps),
    ";",
    opt_space,
    exec_line_as_string(Efirst,Lfirst),
    !,
    makefile_execs(Erest,Lexecs),
    {Lines is Lfirst + Lexecs}.

opt_makefile_deps(T) --> opt_whitespace, makefile_targets(T).
opt_makefile_deps([]) --> opt_whitespace.

makefile_targets([T|Ts]) --> opt_space, makefile_target_string(T), whitespace, makefile_targets(Ts), opt_whitespace.
makefile_targets([T]) --> opt_space, makefile_target_string(T), opt_whitespace.

whitespace_or_linebreak --> "\n", opt_whitespace.
whitespace_or_linebreak --> whitespace.

opt_linebreak --> [].
opt_linebreak --> "\n", opt_whitespace.

makefile_warning_text(S,NL) --> delim(S,[[0'(,0')]],[0')],[0'\\],NL).
makefile_filename_string(S) --> string_from_codes(S," \t\n").

makefile_target_string(S) --> makefile_target_codes(Sc,null), {Sc \= [], string_codes(S,Sc)}.
makefile_target_codes(S,Rterm) --> [0'$,0'(], !, makefile_target_codes(Sv,0')), [0')], makefile_target_codes(St,Rterm), {append([0'$,0'(|Sv],[0')|St],S)}, !.
makefile_target_codes(S,Rterm) --> [0'$,0'{], !, makefile_target_codes(Sv,0'}), [0'}], makefile_target_codes(St,Rterm), {append([0'$,0'{|Sv],[0'}|St],S)}, !.
makefile_target_codes([C|St],Rterm) --> [0'$], makefile_var_char(C), !, makefile_target_codes(St,Rterm), !.
makefile_target_codes([C|St],Rterm) --> [C], {Rterm \= null, \+ member(C,[Rterm,0'\n])}, !, makefile_target_codes(St,Rterm).
makefile_target_codes([C|St],null) --> [C], {\+ member(C,[0'#,0':,0';,0'\s,0'\t,0'\n,0'\\])}, !, makefile_target_codes(St,null).
makefile_target_codes([],_) --> [].

op_string("=") --> "=".
op_string(":=") --> ":=".
op_string("::=") --> ":=".
op_string("?=") --> "?=".
op_string("+=") --> "+=".
op_string("!=") --> "!=".

makefile_execs([E|Es],Lines) --> makefile_exec(E,L), !, {Lines = Lrest + L}, makefile_execs(Es,Lrest).
makefile_execs(Es,Lines) --> comment, !, {Lines = Lrest + 1}, makefile_execs(Es,Lrest).
makefile_execs([],0) --> !.

makefile_exec(E,L) --> "\t", !, exec_line_as_string(E,L).

exec_line([],0) --> call(eos), !.
exec_line([0'\\,0'\n|Cs],Lplus1) --> "\\\n\t", !, exec_line(Cs,L), {Lplus1 is L + 1}.
exec_line([0'\\,0'\n|Cs],Lplus1) --> "\\\n", !, exec_line(Cs,L), {Lplus1 is L + 1}.
exec_line([],1) --> "\n", !.
exec_line([C|Cs],L) --> [C], exec_line(Cs,L).
exec_line_as_string(S,L) --> exec_line(C,L), {string_codes(S,C)}.

line([],0) --> call(eos), !.
line([0'\s|Cs],Lplus1) --> "\\\n", !, line(Cs,L), {Lplus1 is L + 1}.
line([],1) --> "\n", !.
line([],1) --> comment.
line([C|Cs],L) --> [C], line(Cs,L).
line_as_string(S,L) --> line(C,L), {string_codes(S,C)}.
line_as_string(S) --> line_as_string(S,_).

makefile_def_body([],1) --> opt_space, "endef", !, opt_whitespace, "\n".
makefile_def_body(['\n'|Cs],Lplus1) --> ['\n'], !, makefile_def_body(Cs,L), {Lplus1 is L + 1}.
makefile_def_body([C|Cs],Lines) --> [C], makefile_def_body(Cs,Lines).

opt_comment --> comment.
opt_comment --> opt_space, "\n", [].
comment --> opt_space, "#", ignore_line.
ignore_line --> ("\n" ; call(eos)), !.
ignore_line --> [_], ignore_line.
ignore_line --> [].

% due to @triska
merge_unifications(Us1, Us2, Us) :-
        append(Us1, Us2, Us3),
        maplist(eq_pair, Us3, Pairs0),
        keysort(Pairs0, Pairs),
        group_pairs_by_key(Pairs, Groups),
        maplist(vars_all_equal, Groups, Us).

eq_pair(A=B, A-B).

vars_all_equal(Label-[Var|Vars], Label=Var) :-
        maplist(=(Var), Vars).
