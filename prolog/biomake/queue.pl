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
:- discontiguous qsub_dep_arg/3.
:- discontiguous qsub_dep_prefix/2.
:- discontiguous qsub_dep_separator/2.
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
	rule_execs(Rule,Es,Opts),
	(member(qsub_exec(QsubExec),Opts); default_qsub_exec(Engine,QsubExec)),
	(member(qsub_args(QsubArgs),Opts); QsubArgs = ""),
	biomake_private_filename_dir_exists(T,Engine,JobFilename),
	format(string(RemoveJobFile),"rm ~w",[JobFilename]),
	append(Es,[RemoveJobFile],ExecsWithCleanup),
	qsub_job_ids(Engine,DL,DepJobs),
	qsub_script_headers(Engine,DepJobs,Opts,Headers),
	qsub_dep_arg(Engine,DepJobs,DepArg),
	write_script_file(T,Headers,ExecsWithCleanup,Opts,ScriptFilename),
	format(string(QsubCmd),"~w ~w ~w ~w >~w",[QsubExec,QsubArgs,DepArg,ScriptFilename,JobFilename]),
	report("Submitting job: ~w",[QsubCmd],SL,Opts),
	shell(QsubCmd).

qsub_job_ids(Engine,[D|Ds],[N|Ns]) :-
	qsub_job_id(Engine,D,N),
	!,
	qsub_job_ids(Engine,Ds,Ns).
qsub_job_ids(Engine,[_|Ds],Ns) :-
	!,
	qsub_job_ids(Engine,Ds,Ns).
qsub_job_ids(_,[],[]).

qsub_numeric_job_id(Engine,T,N) :-
	biomake_private_filename(T,Engine,JobFilename),
	exists_file(JobFilename),
	phrase_from_file(first_int(N),JobFilename).

qsub_make_dep_arg(_,[],"").
qsub_make_dep_arg(Engine,DepJobs,DepArg) :-
        qsub_dep_prefix(Engine,DepPrefix),
	qsub_dep_separator(Engine,DepSep),
	atomic_list_concat(DepJobs,DepSep,DepJobStr),
	string_concat(DepPrefix,DepJobStr,DepArg).

first_int(N) --> before_first_int(Cs), !, {number_codes(N,Cs)}.
before_first_int([C|Cs]) --> parse_num_code(C), !, first_int_codes(Cs).
before_first_int(Cs) --> [_], before_first_int(Cs).
first_int_codes([C|Cs]) --> parse_num_code(C), !, first_int_codes(Cs).
first_int_codes([]) --> [_], first_int_codes([]).
first_int_codes([]) --> [].

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
	open_script_file(T,ScriptFilename,IO),
	shell_path(Sh),
	maplist(shell_comment,Headers,Comments),
	append(Comments,Execs,Contents),
	concat_string_list(Contents,Str,"\n"),
	format(IO,"#!~w~n~w~n",[Sh,Str]),
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
qsub_dep_prefix(sge,"-hold_jid ").
qsub_dep_separator(sge,",").
qsub_dep_arg(sge,DepJobs,Arg) :- qsub_make_dep_arg(sge,DepJobs,Arg).
qsub_script_headers(sge,_,_,[]).
qsub_job_id(sge,T,N) :- qsub_numeric_job_id(sge,T,N).

% ----------------------------------------
% PBS
% ----------------------------------------

queue_engine(pbs).

run_execs_in_queue(pbs,Rule,SL,Opts) :-
	run_execs_with_qsub(pbs,Rule,SL,Opts).

default_qsub_exec(pbs,"qsub").
qsub_dep_prefix(pbs,"-W depend=afterok:").
qsub_dep_separator(pbs,":").
qsub_dep_arg(pbs,DepJobs,Arg) :- qsub_make_dep_arg(pbs,DepJobs,Arg).
qsub_script_headers(pbs,_,_,[]).
qsub_job_id(pbs,T,N) :-
	biomake_private_filename(T,pbs,JobFilename),
	exists_file(JobFilename),
	read_file_to_string(JobFilename,N,[]).
