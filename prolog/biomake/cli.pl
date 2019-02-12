% * -*- Mode: Prolog -*- */

:- use_module(library(biomake/biomake)).
:- use_module(library(biomake/utils)).
:- use_module(library(biomake/embed)).
:- use_module(library(biomake/sync)).

% ----------------------------------------
% MAIN PROGRAM
% ----------------------------------------

% Declare all debug topics defined in this module
:- nodebug(verbose).
:- nodebug(build).
:- nodebug(md5).

main :-
        current_prolog_flag(argv, Arguments),
        (append(_SytemArgs, [--|Args], Arguments) ; =(Arguments,Args)),
        !,
        parse_args(Args,TmpOpts),
	get_cmd_args(TmpOpts,Opts),
 	add_assignments(Opts),
	bind_special_variables(Opts),
	eval_makefile_syntax_args(Opts2,Opts),
	eval_makespec_syntax_args(Opts3,Opts2),
	consult_makefile(Opts4,Opts3),
	AllOpts = Opts4,
        forall(member(goal(G),AllOpts),
               G),
        forall(member(flush_queue(T),AllOpts),
	       flush_queue_recursive(T,AllOpts)),
	sync_remote_to_cwd(Cwd,AllOpts),
	(build_toplevel(AllOpts)
	-> sync_dir_to_remote(Cwd,AllOpts),
	   halt_success
	;  halt_error).

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

eval_makefile_syntax_args(OptsOut,OptsIn) :-
    bagof(Eval,member(eval_makefile_syntax(Eval),OptsIn),Evals),
    !,
    ensure_loaded(library(biomake/gnumake_parser)),
    eval_makefile_syntax_args(Evals,OptsOut,OptsIn).
eval_makefile_syntax_args(Opts,Opts).
eval_makefile_syntax_args([Eval|Evals],OptsOut,OptsIn) :-
    eval_gnu_makefile(Eval,_,Opts,OptsIn),
    eval_makefile_syntax_args(Evals,OptsOut,Opts).
eval_makefile_syntax_args([],Opts,Opts).

eval_makespec_syntax_args(OptsOut,OptsIn) :-
    bagof(Eval,member(eval_makespec_syntax(Eval),OptsIn),Evals),
    !,
    eval_makespec_syntax_args(Evals,OptsOut,OptsIn).
eval_makespec_syntax_args(Opts,Opts).
eval_makespec_syntax_args([Eval|Evals],OptsOut,OptsIn) :-
    eval_atom_as_makeprog_term(Eval,Opts,OptsIn),
    eval_makespec_syntax_args(Evals,OptsOut,Opts).
eval_makespec_syntax_args([],Opts,Opts).

consult_makefile(AllOpts,Opts) :-
	DefaultMakeprogs = ['Makeprog','makeprog','Makespec.pro','makespec.pro'],
	DefaultGnuMakefiles = ['Makefile','makefile'],
	(member(makeprog(BF),Opts)
	 -> consult_makeprog(BF,AllOpts,Opts);
	 (member(gnu_makefile(F),Opts)
	  -> consult_gnu_makefile(F,AllOpts,Opts);
	  (find_file(DefaultMakeprog,DefaultMakeprogs)
	   -> consult_makeprog(DefaultMakeprog,AllOpts,Opts);
	   (find_file(DefaultGnuMakefile,DefaultGnuMakefiles)
	    -> consult_gnu_makefile(DefaultGnuMakefile,AllOpts,Opts)
	    ; (format("No Makefile found~n"),
	       halt_error))))).

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
parse_args(Args,Opts) :-
        parse_arg(Args,RestArgs,Opt),
        !,
	(Opt = [_|_] -> ArgOpts = Opt; ArgOpts = [Opt]),
	append(ArgOpts,RestOpts,Opts),
        parse_args(RestArgs,RestOpts).
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
	default_qsub_biomake_args(DQ),
	CmdOpts = [biomake_prog(CmdPath),
		   biomake_args(CoreStr),
		   biomake_cwd(CWD),
		   qsub_biomake_args(DQ)],
	append(FlatOpts,CmdOpts,Opts).

arg_from_opts(Arg,Opts) :-
	member(Opt,Opts),
	recover_arg(Arg,Opt).

multi_args(Opts) --> "-", multi_arg(Opts).
multi_arg([Opt|Rest]) --> [C], {char_code('-',H),C\=H,atom_codes(Arg,[H,C])}, !, {parse_arg([Arg],[],Opt)}, !, multi_arg(Rest).
multi_arg([]) --> !.

:- discontiguous parse_arg/3.   % describes how to parse a cmdline arg into an option
:- discontiguous recover_arg/2. % describes how to recover the cmdline arg from the option (finding canonical paths, etc)
:- discontiguous simple_arg/2.  % combines parse_arg & recover_arg for simple options with no parameters
:- discontiguous arg_alias/2.   % specifies an alias for a cmdline arg
:- discontiguous arg_info/3.    % specifies the help text for a cmdline arg

parse_arg([Arg|L],L,Opt) :- simple_arg(Arg,Opt).
recover_arg(Arg,Opt) :- simple_arg(Arg,Opt).

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
        writeln('For more info see http://github.com/evoldoers/biomake'),
        nl,
        halt_success.

parse_arg(['-v'|L],L,null) :- show_version, !.
arg_alias('-v','--version').
arg_info('-v','','Show version').

show_version :-
        writeln('Biomake v0.1.5'),
        writeln('Copyright (C) 2016 Evolutionary Software Foundation, Inc.'),
        writeln('Authors: Chris Mungall, Ian Holmes.'),
        writeln('This is free software; see the source for copying conditions.'),
        writeln('There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A'),
        writeln('PARTICULAR PURPOSE.'),
        halt_success.

simple_arg('-n',dry_run(true)).
arg_alias('-n','--dry-run').
arg_alias('-n','--recon').
arg_alias('-n','--just-print').
arg_info('-n','','Print the commands that would be executed, but do not execute them').

simple_arg('-B',always_make(true)).
arg_alias('-B','--always-make').
arg_info('-B','','Always build fresh target even if dependency is up to date').

parse_arg(['-f',F|L],L,gnu_makefile(F)).
arg_alias('-f','--file').
arg_alias('-f','--makefile').
recover_arg(['-f',Fabs],gnu_makefile(F)) :- absolute_file_name(F,Fabs).
arg_info('-f','GNUMAKEFILE','Use a GNU Makefile as the build specification [default: Makefile]').

parse_arg(['-p',F|L],L,makeprog(F)).
arg_alias('-p','--prog').
arg_alias('-p','--makeprog').
recover_arg(['-p',Fabs],makeprog(F)) :- absolute_file_name(F,Fabs).
arg_info('-p','MAKEPROG','Use MAKEPROG as the (Prolog) build specification [default: Makeprog]').

parse_arg(['-m',Text|L],L,eval_makefile_syntax(Text)).
recover_arg(['-m',Text],eval_makefile_syntax(Text)).
arg_alias('-m','--eval').
arg_alias('-m','--makefile-syntax').
arg_info('-m','STRING','Evaluate STRING as GNU Makefile syntax').

parse_arg(['-P',Text|L],L,eval_makespec_syntax(Text)).
recover_arg(['-P',Text],eval_makespec_syntax(Text)).
arg_alias('-P','--eval-prolog').
arg_alias('-P','--makeprog-syntax').
arg_info('-P','STRING','Evaluate STRING as Prolog Makeprog syntax').

parse_arg(['-I',D|L],L,include_dir(D)).
arg_alias('-I','--include-dir').
recover_arg(['-I',Dabs],include_dir(D)) :- absolute_file_name(D,Dabs).
arg_info('-I','DIR','Specify search directory for included Makefiles').

parse_arg(['--target',T|L],L,toplevel(T)).
arg_info('--target','TARGET','Force biomake to recognize a target even if it looks like an option').

parse_arg(['-T',F|L],L,translate_gnu_makefile(F)).
arg_alias('-T','--translate').
arg_alias('-T','--save-prolog').
arg_info('-T','FILE','Translate GNU Makefile to Prolog Makeprog syntax').

parse_arg(['-W',F|L],L,what_if(Fs)) :- atom_string(F,Fs).
arg_alias('-W','--what-if').
arg_alias('-W','--new-file').
arg_alias('-W','--assume-new').
recover_arg(['-W',F],what_if(F)).
arg_info('-W','TARGET','Pretend that TARGET has been modified').

parse_arg(['-o',F|L],L,old_file(Fs)) :- atom_string(F,Fs).
arg_alias('-o','--old-file').
arg_alias('-o','--assume-old').
recover_arg(['-o',F],old_file(F)).
arg_info('-o','TARGET','Do not remake TARGET, or remake anything on account of it').

simple_arg('-k',keep_going_on_error(true)).
arg_alias('-k','--keep-going').
arg_info('-k','','Keep going after error').

simple_arg('-S',keep_going_on_error(false)).
arg_alias('-S','--no-keep-going').
arg_alias('-S','--stop').
arg_info('-S','','Stop after error').

simple_arg('-t',touch_only(true)).
arg_alias('-t','--touch').
arg_info('-t','','Touch files (& update MD5 hashes, if appropriate) instead of running recipes').

parse_arg(['-N'|L],L,no_deps(true)).
arg_alias('-N','--no-dependencies').
arg_info('-N','','Do not test or rebuild dependencies').

parse_arg(['-D',Var,Val|L],L,assignment(Var,Val)).
arg_alias('-D','--define').
parse_arg([VarEqualsVal|L],L,assignment(Var,Val)) :-
    string_codes(VarEqualsVal,C),
    phrase(makefile_assign(Var,Val),C).
recover_arg(VarEqualsVal,assignment(Var,Val)) :-
    format(string(VarEqualsVal),"--define ~w ~q",[Var,Val]).
arg_info('-D','Var Val','Assign Makefile variables from command line').
arg_info('Var=Val','','Alternative syntax for \'-D Var Val\'').

makefile_assign(Var,Val) --> makefile_var(Var), "=", makefile_val(Val).
makefile_var(A) --> makefile_var_atom_from_codes(A).
makefile_val(S) --> "\"", string_from_codes(S,"\""), "\"".
makefile_val(S) --> string_from_codes(S," ").

% ----------------------------------------
% ESOTERIC FEATURES
% ----------------------------------------

simple_arg('-s',silent(true)).
arg_alias('-s','--quiet').
arg_alias('-s','--silent').
arg_info('-s','','Silent operation; do not print recipes as they are executed').

simple_arg('--one-shell',oneshell(true)).
arg_info('--one-shell','','Run recipes in single shell (loosely equivalent to GNU Make\'s .ONESHELL)').

% ----------------------------------------
% SYNC TO REMOTE STORAGE
% ----------------------------------------

parse_arg(['-y',URIs|L],L,sync(URI)) :-
	atom_string(URI,URIs),
	!.
arg_alias('-y','--sync').
arg_alias('-y','--sync-dir').
recover_arg(['-y',URI],sync(URI)).
arg_info('-y','URI','Synchronize current working directory to a remote URI. If no --sync-exec is specified, S3-form URIs (s3://mybucket/my/path) are handled using the AWS CLI tool; other URIs will be passed to rsync.').

parse_arg(['-x',Es|L],L,sync_exec(E)) :-
	atom_string(E,Es),
	!.
arg_alias('-x','--sync-exec').
recover_arg(['-x',E],sync_exec(E)).
arg_info('-x','COMMAND','Specify executable for --sync.').


% ----------------------------------------
% MD5 CHECKSUMS
% ----------------------------------------

parse_arg(['-H'|L],L,md5(true)) :- ensure_loaded(library(biomake/md5hash)), !.
arg_alias('-H','--md5-hash').
arg_alias('-H','--md5-checksum').
recover_arg(['-H'],md5(true)).
arg_info('-H','','Use MD5 hashes instead of timestamps').

simple_arg('-C',no_md5_cache(true)).
arg_alias('-C','--no-md5-cache').
arg_info('-C','','Recompute MD5 checksums whenever biomake is restarted').

simple_arg('-M',ignore_md5_timestamp(true)).
arg_alias('-M','--no-md5-timestamp').
arg_info('-M','','Do not recompute MD5 checksums when timestamps appear stale').

% ----------------------------------------
% QUEUES
% ----------------------------------------

parse_arg(['-Q',Qs|L],L,queue(Q)) :-
        ensure_loaded(library(biomake/queue)),
	atom_string(Q,Qs),
	queue_engine(Q),
	!.
parse_arg(['-Q',Qs|L],L,null) :- format("Warning: unknown queue '~w'~n",Qs), !.
arg_alias('-Q','--queue-engine').
arg_info('-Q','ENGINE','Queue recipes using ENGINE (supported: poolq,sge,pbs,slurm,test)').

parse_arg(['-j',Jobs|L],L,poolq_threads(NJobs)) :- atom_number(Jobs,NJobs).
arg_alias('-j','--jobs').
arg_info('-j','JOBS','Number of job threads (poolq engine)').

parse_arg(['--qsub-exec',X|L],L,qsub_exec(X)).
arg_alias('--qsub-exec','--sbatch-exec').
arg_info('--qsub-exec','PATH','Path to qsub (sge,pbs) or sbatch (slurm)').

parse_arg(['--qdel-exec',X|L],L,qsub_exec(X)).
arg_alias('--qdel-exec','--scancel-exec').
arg_info('--qdel-exec','PATH','Path to qdel (sge,pbs) or scancel (slurm)').

parse_arg(['--queue-args',X|L],L,queue_args(X)).
arg_info('--queue-args','\'ARGS\'','Queue-specifying arguments for qsub/qdel (sge,pbs) or sbatch/scancel (slurm)').

parse_arg(['--qsub-args',X|L],L,qsub_args(X)).
arg_alias('--qsub-args','--sbatch-args').
arg_info('--qsub-args','\'ARGS\'','Additional arguments for qsub (sge,pbs) or sbatch (slurm)').

parse_arg(['--qsub-use-biomake'|L],L,qsub_use_biomake(true)).
arg_alias('--qsub-use-biomake','--sbatch-use-biomake').
arg_info('--qsub-use-biomake','','Force qsub/sbatch to always call biomake recursively').

parse_arg(['--qsub-biomake-args',X|L],L,qsub_args(X)).
parse_arg(['--qsub-biomake-args',X|L],L,qsub_args(X)).
arg_alias('--qsub-biomake-args','--sbatch-biomake-args').
default_qsub_biomake_args('-N').
arg_info('--qsub-biomake-args','\'ARGS\'',S) :-
    default_qsub_biomake_args(Default),
    format(atom(S),'Arguments passed recursively to biomake by qsub/sbatch (default: ~q)',[Default]).

parse_arg(['--qsub-header',X|L],L,qsub_header(X)).
arg_alias('--qsub-header','--sbatch-header').
arg_info('--qsub-header','\'HEADER\'','Header for qsub (sge,pbs) or sbatch (slurm)').

parse_arg(['--qsub-header-file',X|L],L,qsub_header_file(X)).
arg_alias('--qsub-header-file','--sbatch-header-file').
arg_info('--qsub-header-file','\'FILENAME\'','Header file for qsub (sge,pbs) or sbatch (slurm)').

parse_arg(['--qdel-args',X|L],L,qdel_args(X)).
arg_alias('--qdel-args','--scancel-args').
arg_info('--qdel-args','\'ARGS\'','Additional arguments for qdel (sge,pbs) or scancel (slurm)').

parse_arg(['--flush',X|L],L,flush_queue(X)).
arg_alias('--flush','--qsub-flush').
arg_info('--flush','<target or directory>','Erase all jobs for given target/dir').

% ----------------------------------------
% DEBUGGING
% ----------------------------------------

parse_arg(['-d'|L],L,null) :- debug(verbose), debug(build), set_prolog_flag(verbose,normal).
arg_info('-d','','[developers] Print debugging messages. Equivalent to \'--debug verbose\'').

parse_arg(['--debug',D|L],L,null) :- debug(D), set_prolog_flag(verbose,normal).
arg_info('--debug','MSG','[developers] Richer debugging messages. MSG can be verbose, bindrule, build, pattern, makefile, makeprog, md5...').

parse_arg(['--trace',Pred|L],L,null) :- trace(Pred), !.
arg_info('--trace','PREDICATE','[developers] Print debugging trace for given predicate').

parse_arg(['--no-backtrace'|L],L,null) :- disable_backtrace, !.
arg_info('--no-backtrace','','[developers] Do not print a backtrace on error').
