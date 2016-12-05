% * -*- Mode: Prolog -*- */

:- module(queue,
          [
              queue_engine/1,
	      script_filename/2,
	      write_script_file/4,
	      write_script_file/5,
	      run_execs_in_queue/4
          ]).

:- use_module(library(readutil)).

:- use_module(library(biomake/biomake)).
:- use_module(library(biomake/utils)).

:- discontiguous queue_engine/1.
:- discontiguous run_execs_in_queue/4.
:- discontiguous default_qsub_exec/2.
:- discontiguous default_qdel_exec/2.
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
	(member(qsub_exec(QsubExec),Opts); default_qsub_exec(Engine,QsubExec)),
	(member(qsub_args(QsubArgs),Opts); QsubArgs = ""),
	(member(queue_args(QArgs),Opts); QArgs = ""),
	qsub_extra_args(Engine,ExtraArgs),
	biomake_private_filename_dir_exists(T,Engine,JobFilename),
	format(string(RemoveJobFile),"rm ~w",[JobFilename]),
	qsub_rule_execs(Rule,Es,Opts),
	append(Es,[RemoveJobFile],ExecsWithCleanup),
	qsub_job_ids(Engine,DL,DepJobs),
	qsub_script_headers(Engine,DepJobs,Opts,Headers),
	qsub_dep_arg(Engine,DepJobs,DepArg),
	!,
	write_script_file(T,Headers,ExecsWithCleanup,Opts,ScriptFilename),
	format(string(QsubCmd),"~w ~w ~w ~w ~w ~w >~w",[QsubExec,QArgs,QsubArgs,DepArg,ExtraArgs,ScriptFilename,JobFilename]),
	report("Submitting job: ~w",[QsubCmd],SL,Opts),
	shell(QsubCmd).

qsub_rule_execs(Rule,[Chdir,Biomake],Opts) :-
	qsub_use_biomake(Opts),
	!,
	rule_target(Rule,T,Opts),
	member(biomake_cwd(Dir),Opts),
	member(biomake_prog(Prog),Opts),
	member(biomake_args(Args),Opts),
	format(string(Chdir),"cd ~w",[Dir]),
	format(string(Biomake),"~w ~w ~w",[Prog,Args,T]).

qsub_rule_execs(Rule,Es,Opts) :-
	rule_execs(Rule,Es,Opts).

qsub_use_biomake(Opts) :-
	member(md5(true),Opts).

qsub_job_ids(Engine,[D|Ds],[N|Ns]) :-
	qsub_job_id(Engine,D,N),
	!,
	qsub_job_ids(Engine,Ds,Ns).
qsub_job_ids(Engine,[_|Ds],Ns) :-
	!,
	qsub_job_ids(Engine,Ds,Ns).
qsub_job_ids(_,[],[]).

qsub_kill(Engine,T,SL,Opts) :-
	qsub_job_id(Engine,T,Id),
	(member(qdel_exec(QdelExec),Opts); default_qdel_exec(Engine,QdelExec)),
	(member(qdel_args(QdelArgs),Opts); QdelArgs = ""),
	(member(queue_args(QArgs),Opts); QArgs = ""),
	format(string(QdelCmd),"~w ~w ~w ~w",[QdelExec,QArgs,QdelArgs,Id]),
	report("Killing previous job: ~w",[QdelCmd],SL,Opts),
	(shell(QdelCmd); true),
	biomake_private_filename(T,Engine,JobFilename),
	(exists_file(JobFilename) -> delete_file(JobFilename); true).
qsub_kill(_,_,_,_).

qsub_numeric_job_id(Engine,T,Id) :-
	biomake_private_filename(T,Engine,JobFilename),
	exists_file(JobFilename),
	phrase_from_file(first_int(Id),JobFilename).

first_int(N) --> before_first_int(Cs), !, {number_codes(N,Cs)}.
before_first_int([C|Cs]) --> parse_num_code(C), !, first_int_codes(Cs).
before_first_int(Cs) --> [_], before_first_int(Cs).
first_int_codes([C|Cs]) --> parse_num_code(C), !, first_int_codes(Cs).
first_int_codes([]) --> [_], first_int_codes([]).
first_int_codes([]) --> [].

qsub_generic_job_id(Engine,T,Id) :-
	biomake_private_filename(T,Engine,JobFilename),
	exists_file(JobFilename),
	phrase_from_file(nl_to_spc(Id),JobFilename).

nl_to_spc(S) --> nl_to_spc_codes(Cs), !, {string_codes(S,Cs)}.
nl_to_spc_codes([32|Cs]) --> [10], !, nl_to_spc_codes(Cs).
nl_to_spc_codes([C|Cs]) --> [C], nl_to_spc_codes(Cs).
nl_to_spc_codes([]) --> [].

qsub_make_dep_arg(_,[],"").
qsub_make_dep_arg(Engine,DepJobs,DepArg) :-
        qsub_dep_arg_prefix(Engine,DepArgPrefix),
        qsub_dep_prefix(Engine,DepPrefix),
	qsub_dep_separator(Engine,DepSep),
	string_concat(DepSep,DepPrefix,SepPrefix),
	atomic_list_concat(DepJobs,SepPrefix,DepJobStr),
	format(string(DepArg),"~w~w~w",[DepArgPrefix,DepPrefix,DepJobStr]).

% ----------------------------------------
% WRITING COMMANDS TO SCRIPT FILES
% ----------------------------------------

script_filename(Target,Filename) :-
	biomake_private_filename(Target,"script",Filename).

open_script_file(Target,Filename,Stream) :-
	open_biomake_private_file(Target,"script",Filename,Stream).

write_script_file(T,Es,Opts,ScriptFilename) :-
	write_script_file(T,[],Es,Opts,ScriptFilename).

write_script_file(T,Headers,Es,Opts,ScriptFilename) :-
	member(oneshell(true),Opts),
	!,
	write_script_file_contents(T,Headers,Es,Opts,ScriptFilename).

write_script_file(T,Headers,Es,Opts,ScriptFilename) :-
	maplist(shell_wrap,Es,Execs),
	write_script_file_contents(T,Headers,Execs,Opts,ScriptFilename).

write_script_file_contents(T,Headers,Execs,_Opts,ScriptFilename) :-
	working_directory(CWD,CWD),
	open_script_file(T,ScriptFilename,IO),
	shell_path(Sh),
	maplist(shell_comment,Headers,Comments),
	append(Execs,Comments,Contents),
	concat_string_list(Contents,Str,"\n"),
	format(IO,"#!~w~ncd ~w~n~w~n",[Sh,CWD,Str]),
	close(IO),
	format(string(Chmod),"chmod +x ~w",[ScriptFilename]),
	shell(Chmod).

% ----------------------------------------
% Test queue engine (just runs shell)
% ----------------------------------------

queue_engine(test).

run_execs_in_queue(test,Rule,SL,Opts) :-
	run_execs_in_script(Rule,SL,Opts).

% ----------------------------------------
% Sun Grid Engine
% ----------------------------------------

queue_engine(sge).

run_execs_in_queue(sge,Rule,SL,Opts) :-
	run_execs_with_qsub(sge,Rule,SL,Opts).

default_qsub_exec(sge,"qsub").
default_qdel_exec(sge,"qdel").
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

run_execs_in_queue(pbs,Rule,SL,Opts) :-
	run_execs_with_qsub(pbs,Rule,SL,Opts).

default_qsub_exec(pbs,"qsub").
default_qdel_exec(pbs,"qdel").
qsub_dep_arg_prefix(pbs,"-W depend=").
qsub_dep_prefix(pbs,"afterok:").
qsub_dep_separator(pbs,",").
qsub_dep_arg(pbs,DepJobs,Arg) :- qsub_make_dep_arg(pbs,DepJobs,Arg).
qsub_extra_args(pbs,"").
qsub_script_headers(pbs,_,_,[]).
qsub_job_id(pbs,T,N) :- qsub_generic_job_id(pbs,T,N).

% ----------------------------------------
% SLURM
% ----------------------------------------

queue_engine(slurm).

run_execs_in_queue(slurm,Rule,SL,Opts) :-
	run_execs_with_qsub(slurm,Rule,SL,Opts).

default_qsub_exec(slurm,"sbatch").
default_qdel_exec(pbs,"scancel").
qsub_dep_arg_prefix(slurm,"--dependency=").
qsub_dep_prefix(slurm,"afterok:").
qsub_dep_separator(slurm,",").
qsub_dep_arg(slurm,DepJobs,Arg) :- qsub_make_dep_arg(slurm,DepJobs,Arg).
qsub_extra_args(slurm,"-parsable").
qsub_script_headers(slurm,_,_,[]).
qsub_job_id(slurm,T,N) :- qsub_generic_job_id(slurm,T,N).
