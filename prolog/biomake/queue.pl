% * -*- Mode: Prolog -*- */

:- module(queue,
          [
              queue_engine/1,
	      script_filename/2,
	      write_script_file/4,
	      write_script_file/5,
	      run_execs_in_queue/4
          ]).

:- use_module(library(biomake/biomake)).
:- use_module(library(biomake/utils)).

:- discontiguous run_execs_in_queue/4.
:- discontiguous default_qsub_exec/2.
:- discontiguous qsub_dep_arg/3.
:- discontiguous qsub_dep_prefix/2.
:- discontiguous qsub_dep_separator/2.

% ----------------------------------------
% SUPPORTED QUEUE ENGINES
% ----------------------------------------

queue_engine(test).
queue_engine(sge).

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
	job_numbers(Engine,DL,DepJobs),
	qsub_script_headers(Engine,DepJobs,Headers),
	qsub_dep_arg(Engine,DepJobs,DepArg),
	write_script_file(T,Headers,ExecsWithCleanup,Opts,ScriptFilename),
	format(string(QsubCmd),"~w ~w ~w ~w >~w",[QsubExec,QsubArgs,DepArg,ScriptFilename,JobFilename]),
	report("Submitting job: ~w",[QsubCmd],SL,Opts),
	shell(QsubCmd).

job_numbers(Engine,[D|Ds],[N|Ns]) :-
	job_number(Engine,D,N),
	!,
	job_numbers(Engine,Ds,Ns).
job_numbers(Engine,[_|Ds],Ns) :-
	!,
	job_numbers(Engine,Ds,Ns).
job_numbers(_,[],[]).

job_number(Engine,T,N) :-
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

run_execs_in_queue(test,Rule,SL,Opts) :-
	run_execs_in_script(Rule,SL,Opts).

% ----------------------------------------
% Sun Grid Engine
% ----------------------------------------

default_qsub_exec(sge,"qsub").
qsub_dep_prefix(sge,"-hold_jid ").
qsub_dep_separator(sge,",").
qsub_dep_arg(sge,DepJobs,Arg) :- qsub_make_dep_arg(sge,DepJobs,Arg).
qsub_script_headers(sge,_,[]).

run_execs_in_queue(sge,Rule,SL,Opts) :-
	run_execs_with_qsub(sge,Rule,SL,Opts).
