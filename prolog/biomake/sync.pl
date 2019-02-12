% * -*- Mode: Prolog -*- */

:- module(sync,
          [
	      sync_remote_to_cwd/2,
	      sync_remote_to_dir/2,
	      sync_dir_to_remote/2
          ]).

:- use_module(library(biomake/biomake)).

% ----------------------------------------
% SYNC TO/FROM REMOTE STORAGE
% ----------------------------------------

sync_remote_to_cwd(Dir,Opts) :-
    working_directory(Cwd,Cwd),
    absolute_file_name(Cwd,Dir),
    sync_remote_to_dir(Dir,Opts).

sync_remote_to_dir(Dir,Opts) :-
    get_opt(sync,URI,Opts),
    !,
    sync_exec(Exec,URI,Opts),
    ends_with_slash(URI,URISlash),
    ends_with_slash(Dir,DirSlash),
    verbose_report('Syncing from ~w to ~w',[URISlash,DirSlash],Opts),
    format(string(SyncCmd),"~w ~w ~w",[Exec,URISlash,DirSlash]),
    shell(SyncCmd).

sync_remote_to_dir(_Dir,_Opts).

sync_dir_to_remote(Dir,Opts) :-
    get_opt(sync,URI,Opts),
    !,
    sync_exec(Exec,URI,Opts),
    ends_with_slash(URI,URISlash),
    ends_with_slash(Dir,DirSlash),
    verbose_report('Syncing from ~w to ~w',[DirSlash,URISlash],Opts),
    format(string(SyncCmd),"~w ~w ~w",[Exec,DirSlash,URISlash]),
    shell(SyncCmd).

sync_dir_to_remote(_Dir,_Opts).

ends_with_slash(S,S) :- string_concat(_,"/",S), !.
ends_with_slash(S,Ss) :- string_concat(S,"/",Ss).

sync_exec(Exec,_URI,Opts) :-
    get_opt(sync_exec,Exec,Opts),
    !.

sync_exec(Exec,URI,_Opts) :-
    string_concat("s3://",_,URI),
    s3_sync_exec(Exec),
    !.

sync_exec(Exec,_URI,_Opts) :-
    rsync_exec(Exec).

s3_sync_exec("aws s3 sync --delete").

rsync_exec("rsync -r --delete").
