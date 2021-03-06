% * -*- Mode: Prolog -*- */

:- module(queue,
          [
              queue_engine/1,
              init_queue/2,
              release_queue/1,
	      write_script_file/4,
	      write_script_file/7,
	      run_execs_in_queue/4,
	      flush_queue_recursive/2
          ]).

:- use_module(library(readutil)).

:- use_module(library(biomake/biomake)).
:- use_module(library(biomake/utils)).

:- discontiguous queue_engine/1.
:- discontiguous init_queue/2.
:- discontiguous release_queue/1.
:- discontiguous run_execs_in_queue/4.
:- discontiguous default_qsub_exec/2.
:- discontiguous default_qdel_exec/2.
:- discontiguous qsub_output_arg/3.
:- discontiguous qsub_error_arg/3.
:- discontiguous qsub_dep_arg/3.
:- discontiguous qsub_dep_arg_prefix/2.
:- discontiguous qsub_dep_prefix/2.
:- discontiguous qsub_dep_separator/2.
:- discontiguous qsub_extra_args/2.
:- discontiguous qsub_script_headers/4.
:- discontiguous qsub_job_id/3.

% ----------------------------------------
% SUPPORTED QUEUE ENGINES
% ----------------------------------------

% ----------------------------------------
% GENERIC JOB SUBMISSION
% ----------------------------------------

run_execs_with_qsub(Engine,Rule,SL,Opts) :-
	rule_target(Rule,T,Opts),
        rule_dependencies(Rule,DL,Opts),
	qsub_kill(Engine,T,SL,Opts),
	(get_opt(qsub_exec,QsubExec,Opts); default_qsub_exec(Engine,QsubExec)),
	(get_opt(qsub_args,QsubArgs,Opts); QsubArgs = ""),
	(get_opt(queue_args,QArgs,Opts); QArgs = ""),
	(get_opt(qsub_header,QsubHeader,Opts); QsubHeader = ""),
	(get_opt(qsub_header_file,QsubHeaderFile,Opts)
	 -> file_contents(QsubHeaderFile,QsubHeaderFileContents)
	 ; QsubHeaderFileContents = ""),
	qsub_extra_args(Engine,ExtraArgs),
	bindvar_rule('QsubArgs',Rule,Opts,RuleQsubArgs),
	bindvar_rule('QsubHeader',Rule,Opts,RuleQsubHeader),
	bindvar_rule('QsubHeaderFile',Rule,Opts,RuleQsubHeaderFile),
	file_contents(RuleQsubHeaderFile,RuleQsubHeaderFileContents),
	biomake_private_filename_dir_exists(T,[Engine,"job"],JobFilename),
	format(string(RemoveJobFile),"rm ~w",[JobFilename]),
	qsub_rule_execs(Rule,Es,Opts),
	qsub_job_ids(Engine,DL,DepJobs),
	qsub_script_headers(Engine,DepJobs,Opts,Headers),
	append(Headers,["","# Main script"],HeadersWithComment),
	qsub_dep_arg(Engine,DepJobs,DepArg),
	biomake_private_filename_dir_exists(T,[Engine,"out"],OutPath),
	qsub_output_arg(Engine,OutPath,OutArg),
	biomake_private_filename_dir_exists(T,[Engine,"err"],ErrPath),
	qsub_error_arg(Engine,ErrPath,ErrArg),
	!,
	write_script_file(T,
			  ["# Header from QsubHeader variable",
			   RuleQsubHeader,
			   "# Header from --qsub-header",
			   QsubHeader,
			   "# Contents of header file specified by QsubHeaderFile variable",
			   RuleQsubHeaderFileContents,
			   "# Contents of header file specified by --qsub-header-file",
			   QsubHeaderFileContents,
			   "# Generic headers" | HeadersWithComment],
			  Es,
			  [RemoveJobFile],
			  Opts,
			  [Engine],
			  ScriptFilename),
	format(string(QsubCmd),"~w ~w ~w ~w ~w ~w ~w ~w ~w >~w",[QsubExec,OutArg,ErrArg,QArgs,QsubArgs,RuleQsubArgs,DepArg,ExtraArgs,ScriptFilename,JobFilename]),
	report("Submitting job: ~w",[QsubCmd],Opts),
	shell(QsubCmd).

qsub_rule_execs(Rule,[Chdir,Biomake],Opts) :-
	qsub_use_biomake(Opts),
	!,
	rule_target(Rule,T,Opts),
	get_opt(biomake_cwd,Dir,Opts),
	get_opt(biomake_prog,Prog,Opts),
	get_opt(biomake_args,Args,Opts),
	get_opt(qsub_biomake_args,QArgs,Opts),
	format(string(Chdir),"cd ~w",[Dir]),  % probably redundant: write_script_file starts with a cd to CWD
	format(string(Biomake),"~w ~w ~w ~w",[Prog,Args,QArgs,T]).

qsub_rule_execs(Rule,Es,Opts) :-
	rule_execs(Rule,Es,Opts).

file_contents(Filename,String) :-
        Filename \= '',
	exists_file(Filename),
	read_file_to_codes(Filename,Codes,[]),
	string_codes(String,Codes),
	!.
file_contents(_,"").

qsub_use_biomake(Opts) :-
	get_opt(qsub_use_biomake,true,Opts).

qsub_job_ids(Engine,[D|Ds],[N|Ns]) :-
	qsub_job_id(Engine,D,N),
	!,
	qsub_job_ids(Engine,Ds,Ns).
qsub_job_ids(Engine,[_|Ds],Ns) :-
	!,
	qsub_job_ids(Engine,Ds,Ns).
qsub_job_ids(_,[],[]).

qsub_numeric_job_id(Engine,T,Id) :-
	biomake_private_filename(T,[Engine,"job"],JobFilename),
	exists_file(JobFilename),
	phrase_from_file(first_int(Id),JobFilename).

first_int(N) --> before_first_int(Cs), !, {number_codes(N,Cs)}.
before_first_int([C|Cs]) --> parse_num_code(C), !, first_int_codes(Cs).
before_first_int(Cs) --> [_], before_first_int(Cs).
first_int_codes([C|Cs]) --> parse_num_code(C), !, first_int_codes(Cs).
first_int_codes([]) --> [_], first_int_codes([]).
first_int_codes([]) --> [].

qsub_make_dep_arg(_,[],"").
qsub_make_dep_arg(Engine,DepJobs,DepArg) :-
        qsub_dep_arg_prefix(Engine,DepArgPrefix),
        qsub_dep_prefix(Engine,DepPrefix),
	qsub_dep_separator(Engine,DepSep),
	string_concat(DepSep,DepPrefix,SepPrefix),
	atomic_list_concat(DepJobs,SepPrefix,DepJobStr),
	format(string(DepArg),"~w~w~w",[DepArgPrefix,DepPrefix,DepJobStr]).

% ----------------------------------------
% KILLING JOBS
% ----------------------------------------

qsub_kill(Engine,T,SL,Opts) :-
	qsub_job_id(Engine,T,Id),
	(get_opt(qdel_exec,QdelExec,Opts); default_qdel_exec(Engine,QdelExec)),
	(get_opt(qdel_args,QdelArgs,Opts); QdelArgs = ""),
	(get_opt(queue_args,QArgs,Opts); QArgs = ""),
	format(string(QdelCmd),"~w ~w ~w ~w",[QdelExec,QArgs,QdelArgs,Id]),
	verbose_report("Killing previous job: ~w",[QdelCmd],SL,Opts),
	(shell(QdelCmd); true),
	biomake_private_filename(T,[Engine,"job"],JobFilename),
	(exists_file(JobFilename) -> delete_file(JobFilename); true).
qsub_kill(_,_,_,_).

flush_queue_recursive(Dir,Opts) :-
	get_opt(queue,Engine,Opts),
	absolute_file_name(Dir,AbsDir),  % guard against Dir='.'
	flush_queue_recursive(Engine,AbsDir,[],Opts).

flush_queue_recursive(_,X,_,_) :-
	atom_chars(X,['.'|_]),
	!.
flush_queue_recursive(Engine,Dir,SL,Opts) :-
	exists_directory(Dir),
	!,
	verbose_report("Scanning ~w",[Dir],SL,Opts),
	directory_files(Dir,Files),
	forall(member(File,Files),
	       flush_queue_recursive(Engine,File,[Dir|SL],Opts)).
flush_queue_recursive(Engine,File,SL,Opts) :-
	qsub_kill(Engine,File,SL,Opts),
	!.
flush_queue_recursive(_,_,_,_).

% ----------------------------------------
% WRITING COMMANDS TO SCRIPT FILES
% ----------------------------------------

write_script_file(T,Es,Opts,ScriptFilename) :-
	write_script_file(T,[],Es,[],Opts,[],ScriptFilename).

write_script_file(T,Headers,Es,Cleanup,Opts,Subdirs,ScriptFilename) :-
        (get_opt(oneshell,true,Opts)
	 -> maplist(echo_wrap,Es,ShellExecs)
	 ; maplist(shell_echo_wrap,Es,ShellExecs)),
	maplist(shell_wrap,Cleanup,ShellCleanup),
	append(ShellExecs,ShellCleanup,ExecsWithCleanup),
	write_script_file_contents(T,Headers,ExecsWithCleanup,Opts,Subdirs,ScriptFilename).

write_script_file_contents(T,Headers,Execs,Opts,Subdirs,ScriptFilename) :-
	working_directory(CWD,CWD),
	open_script_file(T,Subdirs,ScriptFilename,IO),
	shell_path(Sh),
	wrap_shell_execs(T,Execs,Opts,Subdirs,ExecStr),
	concat_string_list(Headers,HeaderStr,"\n"),
	format(IO,"#!~w~n~w~ncd ~w~n~w~n",[Sh,HeaderStr,CWD,ExecStr]),
	close(IO),
	format(string(Chmod),"chmod +x ~w",[ScriptFilename]),
	shell(Chmod).

wrap_shell_execs(T,Execs,_Opts,Subdirs,ShellFilename) :-
        shell_var_specified(Sh),
        !,
	open_shell_file(T,Subdirs,ShellFilename,IO),
	concat_string_list(Execs,ExecStr,"\n"),
	format(IO,"#!~w~n~w~n",[Sh,ExecStr]),
	close(IO),
	format(string(Chmod),"chmod +x ~w",[ShellFilename]),
	shell(Chmod).

wrap_shell_execs(_T,Execs,_Opts,_Subdirs,ExecStr) :-
        !,
	join_with_ands(Execs,ExecStr).

join_with_ands(List,Str) :-
        !,
	concat_string_list(List,Str," &&\n").

open_script_file(Target,Subdirs,Filename,Stream) :-
        append(Subdirs,["script"],SubdirsScript),
	open_biomake_private_file(Target,SubdirsScript,Filename,Stream).

open_shell_file(Target,Subdirs,Filename,Stream) :-
        append(Subdirs,["shell"],SubdirsShell),
	open_biomake_private_file(Target,SubdirsShell,Filename,Stream).

% ----------------------------------------
% No queue engine (runs execs immediately)
% ----------------------------------------

queue_engine(none).
init_queue(none,_).
release_queue(none).

run_execs_in_queue(none,Rule,SL,Opts) :-
	run_execs_now(Rule,SL,Opts).

% ----------------------------------------
% Test queue engine (just runs script)
% ----------------------------------------

queue_engine(test).
init_queue(test,_).
release_queue(test).

run_execs_in_queue(test,Rule,SL,Opts) :-
	rule_target(Rule,T,Opts),
        rule_dependencies(Rule,DL,Opts),
	qsub_rule_execs(Rule,Es,Opts),
	write_script_file(T,Es,Opts,Script),
	report_run_exec(Script,T,SL,Opts),
	update_hash(T,DL,Opts).

% ----------------------------------------
% Sun Grid Engine
% ----------------------------------------

queue_engine(sge).
init_queue(sge,_).
release_queue(sge).

run_execs_in_queue(sge,Rule,SL,Opts) :-
	run_execs_with_qsub(sge,Rule,SL,Opts).

default_qsub_exec(sge,"qsub").
default_qdel_exec(sge,"qdel").
qsub_output_arg(sge,F,S) :- format(string(S),"-o ~w",[F]).
qsub_error_arg(sge,F,S) :- format(string(S),"-e ~w",[F]).
qsub_dep_arg_prefix(sge,"-hold_jid ").
qsub_dep_prefix(sge,"").
qsub_dep_separator(sge,",").
qsub_dep_arg(sge,DepJobs,Arg) :- qsub_make_dep_arg(sge,DepJobs,Arg).
qsub_extra_args(sge,"").
qsub_script_headers(sge,_,_,[]).
qsub_job_id(sge,T,N) :- qsub_numeric_job_id(sge,T,N).

% ----------------------------------------
% PBS
% ----------------------------------------

queue_engine(pbs).
init_queue(pbs,_).
release_queue(pbs).

run_execs_in_queue(pbs,Rule,SL,Opts) :-
	run_execs_with_qsub(pbs,Rule,SL,Opts).

default_qsub_exec(pbs,"qsub").
default_qdel_exec(pbs,"qdel").
qsub_output_arg(pbs,F,S) :- format(string(S),"-o ~w",[F]).
qsub_error_arg(pbs,F,S) :- format(string(S),"-e ~w",[F]).
qsub_dep_arg_prefix(pbs,"-W depend=").
qsub_dep_prefix(pbs,"afterok:").
qsub_dep_separator(pbs,",").
qsub_dep_arg(pbs,DepJobs,Arg) :- qsub_make_dep_arg(pbs,DepJobs,Arg).
qsub_extra_args(pbs,"").
qsub_script_headers(pbs,_,_,[]).
qsub_job_id(pbs,T,N) :- qsub_numeric_job_id(pbs,T,N).

% ----------------------------------------
% SLURM
% ----------------------------------------

queue_engine(slurm).
init_queue(slurm,_).
release_queue(slurm).

run_execs_in_queue(slurm,Rule,SL,Opts) :-
	run_execs_with_qsub(slurm,Rule,SL,Opts).

default_qsub_exec(slurm,"sbatch").
default_qdel_exec(slurm,"scancel").
qsub_output_arg(slurm,F,S) :- format(string(S),"-o ~w",[F]).
qsub_error_arg(slurm,F,S) :- format(string(S),"-e ~w",[F]).
qsub_dep_arg_prefix(slurm,"--dependency=").
qsub_dep_prefix(slurm,"afterok:").
qsub_dep_separator(slurm,",").
qsub_dep_arg(slurm,DepJobs,Arg) :- qsub_make_dep_arg(slurm,DepJobs,Arg).
qsub_extra_args(slurm,"--parsable").
qsub_script_headers(slurm,_,_,[]).
qsub_job_id(slurm,T,N) :- qsub_numeric_job_id(slurm,T,N).

% ----------------------------------------
% POOLQ
% ----------------------------------------

:- dynamic poolq_scheduler/1.

default_poolq_threads(4).

queue_engine(poolq).
init_queue(poolq,Opts) :-
	ensure_loaded(library(poolq/poolq)),
	(get_opt(poolq_threads,Size,Opts) ; default_poolq_threads(Size)),
	poolq_create(Scheduler,Size,[]),
	assert(poolq_scheduler(Scheduler)).
release_queue(poolq) :-
	poolq_scheduler(Scheduler),
	poolq_wait(Scheduler,_Status).

run_execs_in_queue(poolq,Rule,SL,Opts) :-
	poolq_scheduler(Scheduler),
	rule_target(Rule,T,Opts),
        rule_dependencies(Rule,DL,Opts),
	include(not_always_make_or_queue,Opts,Opts2),
	poolq_submit_job(Scheduler,build(T,SL,[no_deps(true)|Opts2]),T,DL,[]).

not_always_make_or_queue(always_make(true)) :- !, fail.
not_always_make_or_queue(queue(_)) :- !, fail.
not_always_make_or_queue(_).
