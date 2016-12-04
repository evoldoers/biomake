% * -*- Mode: Prolog -*- */

:- module(queue,
          [
              queue_engine/1,
	      script_filename/2,
	      write_script_file/4,
	      run_execs_in_queue/4
          ]).

:- use_module(library(biomake/biomake)).
:- use_module(library(biomake/utils)).

queue_engine(test).

% ----------------------------------------
% WRITING SCRIPT FILES
% ----------------------------------------

script_filename(Target,Filename) :-
	biomake_private_filename(Target,"script",Filename).

script_filename_mkdir(Target,Filename) :-
	biomake_private_filename_mkdir(Target,"script",Filename).

write_script_file(T,Es,Opts,ScriptFilename) :-
	member(oneshell(true),Opts),
	!,
	write_oneshell_script_file(T,Es,Opts,ScriptFilename).

write_script_file(T,Es,Opts,ScriptFilename) :-
	maplist(shell_wrap,Es,ShellEs),
	write_oneshell_script_file(T,ShellEs,Opts,ScriptFilename).

write_oneshell_script_file(T,Es,_Opts,ScriptFilename) :-
	script_filename_mkdir(T,ScriptFilename),
	open(ScriptFilename,write,IO,[]),
	shell_path(Sh),
	concat_string_list(Es,Estr,"\n"),
	format(IO,"#!~w~n~w~n",[Sh,Estr]),
	close(IO),
	format(string(Chmod),"chmod +x ~w",[ScriptFilename]),
	shell(Chmod).

% ----------------------------------------
% TEST QUEUE
% ----------------------------------------

run_execs_in_queue(test,Rule,SL,Opts) :-
	run_execs_in_script(Rule,SL,Opts).
