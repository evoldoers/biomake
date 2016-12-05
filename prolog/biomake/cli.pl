% * -*- Mode: Prolog -*- */

:- use_module(library(biomake/biomake)).
:- use_module(library(biomake/utils)).
:- use_module(library(biomake/queue)).

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
        parse_args(Args,TmpOpts),
	get_cmd_args(TmpOpts,Opts),
 	add_assignments(Opts),
	consult_makefile(Opts),
        forall(member(goal(G),Opts),
               G),
	(build_toplevel(Opts)
	 -> halt(0)
	 ;  halt(1)).

rabbit(X,Y) :- string_concat(X,Y).

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
	DefaultMakeprogs = ['makespec.pro','Makespec.pro'],
	DefaultGnuMakefiles = ['Makefile'],
	(member(makeprog(BF),Opts)
	 -> consult_makeprog(BF,Opts);
	 (member(gnu_makefile(F),Opts)
	  -> consult_gnu_makefile(F,Opts);
	  (find_file(DefaultMakeprog,DefaultMakeprogs)
	   -> consult_makeprog(DefaultMakeprog,Opts);
	   (find_file(DefaultGnuMakefile,DefaultGnuMakefiles)
	    -> consult_gnu_makefile(DefaultGnuMakefile,Opts))))).

find_file(File,List) :-
    member(File,List),
    exists_file(File),
    !.

% ----------------------------------------
% OPTION PROCESSING
% ----------------------------------------

parse_args([],[]).
parse_args([Alias|Rest],Opt) :-
	arg_alias(Arg,Alias),
	!,
	parse_args([Arg|Rest],Opt).
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

get_cmd_args(FlatOpts,Opts) :-
	(bagof(Arg,arg_from_opts(Arg,FlatOpts),LumpyCore);
	    LumpyCore=[]),
        flatten(LumpyCore,Core),
        concat_string_list_spaced(Core,CoreStr),
	biomake_prog(Cmd),
	absolute_file_name(Cmd,CmdPath),
	working_directory(CWD,CWD),
	Opts = [biomake_prog(CmdPath),biomake_args(CoreStr),biomake_cwd(CWD)|FlatOpts].

arg_from_opts(Arg,Opts) :-
	member(Opt,Opts),
	recover_arg(Arg,Opt).

:- discontiguous parse_arg/3.
:- discontiguous recover_arg/2.
:- discontiguous arg_alias/2.
:- discontiguous arg_info/3.

parse_arg(['--debug',D|L],L,null) :- debug(D), set_prolog_flag(verbose,normal).
arg_info('--debug','MSG','[developers] debugging messages. MSG can be build, pattern, makefile, md5...').

parse_arg(['-n'|L],L,dry_run(true)).
arg_alias('-n','--dry-run').
arg_alias('-n','--recon').
arg_alias('-n','--just-print').
arg_info('-n,--dry-run,--recon,--just-print','','Print the commands that would be executed, but do not execute them').

parse_arg(['-h'|L],L,null) :- show_help, !.
arg_alias('-h','--help').
arg_info('-h,--help','','Show help').

show_help :-
        writeln('biomake [OPTION...] target1 target2...'),
        nl,
        writeln('Options:'),
	forall(arg_info(X,Args,Info),
	       format("~w ~w~n    ~w~n",[X,Args,Info])),
        nl,
        writeln('For more info see http://github.com/cmungall/biomake'),
        nl,
        halt.

parse_arg(['-B'|L],L,always_make(true)).
arg_alias('-B','--always-make').
arg_info('-B,--always-make','','Always build fresh target even if dependency is up to date').

parse_arg(['-p',F|L],L,makeprog(F)).
recover_arg(Arg,makeprog(F)) :- absolute_file_name(F,Fabs), format(string(Arg),"-p ~w",[Fabs]).
arg_info('-p','MAKEPROG','Use MAKEPROG as the (Prolog) build specification [default: Makespec.pro]').

parse_arg(['-f',F|L],L,gnu_makefile(F)).
recover_arg(Arg,gnu_makefile(F)) :- absolute_file_name(F,Fabs), format(string(Arg),"-f ~w",[Fabs]).
arg_info('-f','GNUMAKEFILE','Use a GNU Makefile as the build specification').

parse_arg(['-T',F|L],L,translate_gnu_makefile(F)).
parse_arg(['--translate',F|L],L,translate_gnu_makefile(F)).
arg_info('-T,--translate','Translate GNU Makefile to Prolog Makeprog syntax').

parse_arg(['-l',F|L],L,
          goal( (collect_stored_targets(F,[]),
                 show_stored_targets
                ) )) :-
        ensure_loaded(library(biomake/scan)),
        !.
arg_info('-l','DIRECTORY','Iterates through directory writing metadata on each file found').

parse_arg(['-H'|L],L,md5(true)) :- ensure_loaded(library(biomake/md5hash)), !.
arg_alias('-H','--md5-hash').
recover_arg(['-H'],md5(true)).
arg_info('-H,--md5-hash','','Use MD5 hashes instead of timestamps').

parse_arg(['-q'|L],L,silent(true)).
arg_info('-q,--quiet,--silent','','Silent operation; do not print recipes as they are executed').

parse_arg(['--one-shell'|L],L,oneshell(true)).
arg_info('--one-shell','','Run recipes in single shell (equivalent to GNU make\'s .ONESHELL)').

parse_arg(['-Q',Qs|L],L,queue(Q)) :- string_chars(Qs,Qc), atom_chars(Q,Qc), queue_engine(Q), !.
parse_arg(['-Q',Qs|L],L,null) :- format("Warning: unknown queue '~w'~n",Qs), !.
arg_alias('-Q','--queue-engine').
arg_info('-Q,--queue-engine ENGINE','','Queue recipes using ENGINE (supported engines: test,sge,pbs)').

parse_arg(['--qsub-exec',X|L],L,qsub_exec(X)).
arg_info('--qsub-exec PATH','','Path to qsub (sge,pbs)').

parse_arg(['--queue-args',X|L],L,queue_args(X)).
arg_info('--queue-args "ARGS"','','Queue-specifying arguments for qsub/qdel (sge,pbs) or sbatch/scancel (slurm)').

parse_arg(['--qsub-args',X|L],L,qsub_args(X)).
arg_info('--qsub-args "ARGS"','','Additional arguments for qsub (sge,pbs) or sbatch (slurm)').

parse_arg(['--qdel-args',X|L],L,qdel_args(X)).
arg_info('--qdel-args "ARGS"','','Additional arguments for qdel (sge,pbs) or scancel (slurm)').

parse_arg(['--no-backtrace'|L],L,null) :- assert(no_backtrace), !.
arg_info('-no-backtrace','','Do not print a backtrace on error').

parse_arg([VarEqualsVal|L],L,assignment(Var,Val)) :-
    string_codes(VarEqualsVal,C),
    phrase(makefile_assign(Var,Val),C).
recover_arg([VarEqualsVal],assignment(Var,Val)) :-
    format(string(VarEqualsVal),"~w=~q",[Var,Val]).
arg_info('Var=Val','','Assign Makefile variables from command line').

makefile_assign(Var,Val) --> makefile_var(Var), "=", makefile_val(Val).
makefile_var(A) --> atom_from_codes(A,":= \t\n").
makefile_val(S) --> "\"", string_from_codes(S,"\""), "\"".
makefile_val(S) --> string_from_codes(S," ").

multi_args(Opts) --> "-", multi_arg(Opts).
multi_arg([Opt|Rest]) --> [C], {string_codes("-",[H]),C\=H,atom_codes(Arg,[H,C]),parse_arg([Arg],[],Opt)}, !, multi_arg(Rest).
multi_arg([]) --> !.
