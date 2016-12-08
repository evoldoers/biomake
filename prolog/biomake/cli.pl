% * -*- Mode: Prolog -*- */

:- use_module(library(biomake/biomake)).
:- use_module(library(biomake/utils)).

% ----------------------------------------
% EXCEPTIONS
% ----------------------------------------

:- dynamic no_backtrace/0.
:- dynamic prolog_exception_hook/4.

% Intercept a couple of exceptions that are thrown by the threadpool library
% This is kind of yucky, but only seems to affect our exception-handling code
user:prolog_exception_hook(error(existence_error(thread,_),context(system:thread_property/2,_)),_,_,_) :- !, fail.
user:prolog_exception_hook('$aborted',_,_,_) :- !, fail.

% Default exception handler: show backtrace
user:prolog_exception_hook(E,_,_,_) :-
	format("Exception: ~w~n",[E]),
        (no_backtrace; backtrace(99)),
        !,
        fail.

% ----------------------------------------
% MAIN PROGRAM
% ----------------------------------------

main :-
        current_prolog_flag(argv, Arguments),
        (append(_SytemArgs, [--|Args], Arguments) ; =(Arguments,Args)),
        !,
        parse_args(Args,TmpOpts),
	get_cmd_args(TmpOpts,Opts),
 	add_assignments(Opts),
	consult_makefile(AllOpts,Opts),
        forall(member(goal(G),AllOpts),
               G),
        forall(member(flush_queue(T),AllOpts),
	       flush_queue_recursive(T,AllOpts)),
	(build_toplevel(AllOpts)
	 -> halt(0)
	 ;  halt(1)).

build_toplevel(Opts) :-
	member(toplevel(_),Opts),
	!,
	start_queue(Opts),
	forall(member(toplevel(T),Opts),
               build(T,Opts)),
        finish_queue(Opts).

build_toplevel(Opts) :-
	nonbuild_task_specified(Opts),
	!.

build_toplevel(Opts) :-
	start_queue(Opts),
	build_default(Opts),
        finish_queue(Opts).

nonbuild_task_specified(Opts) :- member(translate_gnu_makefile(_),Opts).
nonbuild_task_specified(Opts) :- member(goal(_),Opts).
nonbuild_task_specified(Opts) :- member(flush_queue(_),Opts).

add_assignments(Opts) :-
        forall(member(assignment(Var,Val),Opts),
	       add_cmdline_assignment((Var = Val))).

consult_makefile(AllOpts,Opts) :-
	DefaultMakeprogs = ['makespec.pro','Makespec.pro'],
	DefaultGnuMakefiles = ['Makefile'],
	(member(makeprog(BF),Opts)
	 -> consult_makeprog(BF,AllOpts,Opts);
	 (member(gnu_makefile(F),Opts)
	  -> consult_gnu_makefile(F,AllOpts,Opts);
	  (find_file(DefaultMakeprog,DefaultMakeprogs)
	   -> consult_makeprog(DefaultMakeprog,AllOpts,Opts);
	   (find_file(DefaultGnuMakefile,DefaultGnuMakefiles)
	    -> consult_gnu_makefile(DefaultGnuMakefile,AllOpts,Opts))))).

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
parse_args([ArgEqualsVal|Rest],Opts) :-
        string_chars(ArgEqualsVal,C),
        phrase(arg_equals_val(Arg,Val),C),
	!,
	parse_args([Arg,Val|Rest],Opts).
parse_args([MultiArgs|Args],Opts) :-
        string_codes(MultiArgs,C),
        phrase(multi_args(MultiOpts),C),
	!,
        append(MultiOpts,RestOpts,Opts),
        parse_args(Args,RestOpts).
parse_args(Args,[Opt|Opts]) :-
        parse_arg(Args,Rest,Opt),
        !,
        parse_args(Rest,Opts).
parse_args([A|Args],[toplevel(A)|Opts]) :-
        parse_args(Args,Opts).

arg_equals_val(Arg,Val) --> arg_chars(Arg), ['='], !, val_chars(Val).
arg_chars(A) --> ['-','-'], char_list(Ac,"="), {atom_chars(A,['-','-'|Ac])}.
val_chars(V) --> atom_from_chars(V,"").

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

multi_args(Opts) --> "-", multi_arg(Opts).
multi_arg([Opt|Rest]) --> [C], {string_codes("-",[H]),C\=H,atom_codes(Arg,[H,C])}, !, {parse_arg([Arg],[],Opt)}, !, multi_arg(Rest).
multi_arg([]) --> !.

:- discontiguous parse_arg/3.
:- discontiguous recover_arg/2.
:- discontiguous arg_alias/2.
:- discontiguous arg_info/3.

% ----------------------------------------
% COMMON OPERATIONS
% ----------------------------------------

parse_arg(['-h'|L],L,null) :- show_help, !.
arg_alias('-h','--help').
arg_info('-h','','Show help').

show_help :-
        writeln('biomake [OPTION...] target1 target2...'),
        nl,
        writeln('Options:'),
	forall(arg_info(X,Args,Info),
	       ((bagof(Alias,arg_alias(X,Alias),AliasList); AliasList = []),
	        atomic_list_concat([X|AliasList],",",AliasStr),
	        format("~w ~w~n    ~w~n",[AliasStr,Args,Info]))),
        nl,
        writeln('For more info see http://github.com/cmungall/biomake'),
        nl,
        halt.

parse_arg(['-n'|L],L,dry_run(true)).
arg_alias('-n','--dry-run').
arg_alias('-n','--recon').
arg_alias('-n','--just-print').
arg_info('-n','','Print the commands that would be executed, but do not execute them').

parse_arg(['-B'|L],L,always_make(true)).
arg_alias('-B','--always-make').
arg_info('-B','','Always build fresh target even if dependency is up to date').

parse_arg(['-p',F|L],L,makeprog(F)).
arg_alias('-p','--prog').
arg_alias('-p','--makeprog').
recover_arg(Arg,makeprog(F)) :- absolute_file_name(F,Fabs), format(string(Arg),"-p ~w",[Fabs]).
arg_info('-p','MAKEPROG','Use MAKEPROG as the (Prolog) build specification [default: Makespec.pro]').

parse_arg(['-f',F|L],L,gnu_makefile(F)).
arg_alias('-f','--file').
arg_alias('-f','--makefile').
recover_arg(Arg,gnu_makefile(F)) :- absolute_file_name(F,Fabs), format(string(Arg),"-f ~w",[Fabs]).
arg_info('-f','GNUMAKEFILE','Use a GNU Makefile as the build specification').

parse_arg(['-I',D|L],L,include_dir(D)).
arg_alias('-I','--include-dir').
recover_arg(Arg,include_dir(D)) :- absolute_file_name(D,Dabs), format(string(Arg),"-I ~w",[Dabs]).
arg_info('-I','DIR','Specify search directory for included Makefiles').

parse_arg(['-T',F|L],L,translate_gnu_makefile(F)).
parse_arg(['--translate',F|L],L,translate_gnu_makefile(F)).
arg_info('-T','FILE','Translate GNU Makefile to Prolog Makeprog syntax').

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

% ----------------------------------------
% ESOTERIC FEATURES
% ----------------------------------------

parse_arg(['-l',F|L],L,
          goal( (collect_stored_targets(F,[]),
                 show_stored_targets
                ) )) :-
        ensure_loaded(library(biomake/scan)),
        !.
arg_info('-l','DIRECTORY','Iterates through directory writing metadata on each file found').

parse_arg(['-q'|L],L,silent(true)).
arg_alias('-q','--quiet').
arg_alias('-q','--silent').
arg_info('-q','','Silent operation; do not print recipes as they are executed').

parse_arg(['--one-shell'|L],L,oneshell(true)).
arg_info('--one-shell','','Run recipes in single shell (equivalent to GNU make\'s .ONESHELL)').

% ----------------------------------------
% MD5 CHECKSUMS
% ----------------------------------------

parse_arg(['-H'|L],L,md5(true)) :- ensure_loaded(library(biomake/md5hash)), !.
arg_alias('-H','--md5-hash').
recover_arg(['-H'],md5(true)).
arg_info('-H','','Use MD5 hashes instead of timestamps').

% ----------------------------------------
% QUEUES
% ----------------------------------------

parse_arg(['-Q',Qs|L],L,queue(Q)) :-
        ensure_loaded(library(biomake/queue)),
	string_chars(Qs,Qc),
	atom_chars(Q,Qc),
	queue_engine(Q),
	!.
parse_arg(['-Q',Qs|L],L,null) :- format("Warning: unknown queue '~w'~n",Qs), !.
arg_alias('-Q','--queue-engine').
arg_info('-Q','ENGINE','Queue recipes using ENGINE (supported: test,sge,pbs,slurm,poolq)').

parse_arg(['-j',Jobs|L],L,(atom_codes(Jobs,Jc),number_codes(NJobs,Jc),poolq_threads(NJobs))).
arg_alias('-j','--jobs').
arg_info('-j','JOBS','Number of job threads (poolq engine)').

parse_arg(['--qsub-exec',X|L],L,qsub_exec(X)).
arg_info('--qsub-exec','PATH','Path to qsub (sge,pbs) or sbatch (slurm)').

parse_arg(['--qdel-exec',X|L],L,qsub_exec(X)).
arg_info('--qdel-exec','PATH','Path to qdel (sge,pbs) or scancel (slurm)').

parse_arg(['--queue-args',X|L],L,queue_args(X)).
arg_info('--queue-args','"ARGS"','Queue-specifying arguments for qsub/qdel (sge,pbs) or sbatch/scancel (slurm)').

parse_arg(['--qsub-args',X|L],L,qsub_args(X)).
arg_info('--qsub-args','"ARGS"','Additional arguments for qsub (sge,pbs) or sbatch (slurm)').

parse_arg(['--qdel-args',X|L],L,qdel_args(X)).
arg_info('--qdel-args','"ARGS"','Additional arguments for qdel (sge,pbs) or scancel (slurm)').

parse_arg(['--flush',X|L],L,flush_queue(X)).
arg_alias('--flush','--qsub-flush').
arg_info('--flush','<target or directory>','Erase all jobs for given target/dir').

% ----------------------------------------
% DEBUGGING
% ----------------------------------------

parse_arg(['--debug',D|L],L,null) :- debug(D), set_prolog_flag(verbose,normal).
arg_info('--debug','MSG','[developers] debugging messages. MSG can be build, pattern, makefile, md5...').

parse_arg(['--trace',Pred|L],L,null) :- trace(Pred), !.
arg_info('--trace','predicate','Print debugging trace for given predicate').

parse_arg(['--no-backtrace'|L],L,null) :- assert(no_backtrace), !.
arg_info('--no-backtrace','','Do not print a backtrace on error').
