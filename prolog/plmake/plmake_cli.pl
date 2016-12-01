% * -*- Mode: Prolog -*- */

:- use_module(library(plmake/plmake)).
:- use_module(library(plmake/utils)).

:- dynamic no_backtrace/0.

user:prolog_exception_hook(_,
                           _, _, _) :-
        no_backtrace; backtrace(99),
        !,
        fail.

main :-
        current_prolog_flag(argv, Arguments),
        (append(_SytemArgs, [--|Args], Arguments) ; =(Arguments,Args)),
        !,
        parse_args(Args,Opts_1),
        flatten(Opts_1,Opts),
	add_assignments(Opts),
	consult_makefile(Opts),
        forall(member(goal(G),Opts),
               G),
	build_toplevel(Opts),
        halt.

build_toplevel(Opts) :-
	member(toplevel(_),Opts),
	!,
	forall(member(toplevel(T),Opts),
               build(T,Opts)).

build_toplevel(Opts) :-
	build_default(Opts).

add_assignments(Opts) :-
        forall(member(assignment(Var,Val),Opts),
	       add_cmdline_assignment((Var = Val))).

consult_makefile(Opts) :-
	DefaultMakeprog = 'makespec.pro',
	DefaultGnuMakefile = 'Makefile',
	(member(makeprog(BF),Opts)
	 -> consult_makeprog(BF,Opts);
	 (member(gnu_makefile(F),Opts)
	  -> consult_gnu_makefile(F,Opts);
	  (exists_file(DefaultMakeprog)
	   -> consult_makeprog(DefaultMakeprog,Opts);
	   consult_gnu_makefile(DefaultGnuMakefile,Opts)))).

% ----------------------------------------
% OPTION PROCESSING
% ----------------------------------------

parse_args([],[]).
parse_args(Args,[Opt|Opts]) :-
        parse_arg(Args,Rest,Opt),
        !,
        parse_args(Rest,Opts).
parse_args([MultiArgs|Args],Opts) :-
    string_codes(MultiArgs,C),
    phrase(multi_args(MultiOpts),C),
    append(MultiOpts,RestOpts,Opts),
    parse_args(Args,RestOpts).
parse_args([A|Args],[toplevel(A)|Opts]) :-
        parse_args(Args,Opts).

:- discontiguous parse_arg/3.
:- discontiguous arg_info/3.

parse_arg(['--debug',D|L],L,null) :- debug(D), set_prolog_flag(verbose,normal).
arg_info('--debug','MSG','[developers] debugging messages. MSG can be build, pattern, makefile, md5...').

parse_arg(['--dry-run'|L],L,dry_run(true)).
parse_arg(['-n'|L],L,dry_run(true)).
arg_info('-n,--dry-run','','Print the commands that would be executed, but do not execute them').

parse_arg(['-h'|L],L,null) :- show_help, !.
parse_arg(['--help'|L],L,null) :- show_help, !.
arg_info('-h,--help','','Show help').

show_help :-
        writeln('plmake [OPTION...] target1 target2...'),
        nl,
        writeln('Options:'),
	forall(arg_info(X,Args,Info),
	       format("~w ~w~n    ~w~n",[X,Args,Info])),
        nl,
        writeln('For more info see http://github.com/cmungall/plmake'),
        nl,
        halt.

parse_arg(['--always-make'|L],L,always_make(true)).
parse_arg(['-B'|L],L,always_make(true)).
arg_info('-B,--always-make','','Always build fresh target even if dependency is up to date').

parse_arg(['-p',F|L],L,makeprog(F)) :- !.
arg_info('-p','MAKEPROG','Use MAKEPROG as the (Prolog) build specification [default: makespec.pro]').

parse_arg(['-f',F|L],L,gnu_makefile(F)).
arg_info('-f','GNUMAKEFILE','Use a GNU Makefile as the build specification').

parse_arg(['-T',F|L],L,translate_gnu_makefile(F)).
parse_arg(['--translate',F|L],L,translate_gnu_makefile(F)).
arg_info('-T,--translate','Translate GNU Makefile to Prolog Makeprog syntax').

parse_arg(['-l',F|L],L,
          goal( (collect_stored_targets(F,[]),
                 show_stored_targets
                ) )) :-
        ensure_loaded(library(plmake/plmake_db)),
        !.
arg_info('-l','DIRECTORY','Iterates through directory writing metadata on each file found').

parse_arg(['-H'|L],L,md5(true)) :- ensure_loaded(library(plmake/md5)), !.
parse_arg(['--md5-hash'|L],L,md5(true)) :- ensure_loaded(library(plmake/md5)), !.
arg_info('-H,--md5-hash','','Use MD5 hashes instead of timestamps').

parse_arg(['--no-backtrace'|L],L,quiet(true)) :- assert(no_backtrace), !.
arg_info('-no-backtrace','','Do not print a backtrace on error').

parse_arg([VarEqualsVal|L],L,assignment(Var,Val)) :-
    string_codes(VarEqualsVal,C),
    phrase(makefile_assign(Var,Val),C).
arg_info('Var=Val','','Assign Makefile variables from command line').

makefile_assign(Var,Val) --> makefile_var(Var), "=", makefile_val(Val).
makefile_var(A) --> atom_from_codes(A,":= \t\n").
makefile_val(S) --> "\"", string_from_codes(S,"\""), "\"".
makefile_val(S) --> string_from_codes(S," ").

multi_args(Opts) --> "-", multi_arg(Opts).
multi_arg([Opt|Rest]) --> [C], {string_codes("-",[H]),C\=H,atom_codes(Arg,[H,C]),parse_arg([Arg],[],Opt)}, !, multi_arg(Rest).
multi_arg([]) --> !.
