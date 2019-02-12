% * -*- Mode: Prolog -*- */

:- module(sync,
          [
	      sync_cwd_to_remote/1,
	      sync_remote_to_cwd/1
          ]).

% ----------------------------------------
% SYNC TO/FROM REMOTE STORAGE
% ----------------------------------------

sync_cwd_to_remote(Opts) :-
    get_opt(sync,URI,Opts),
    !,
    sync_exec(Exec,URI,Opts),
    format(string(SyncCmd),"~w . ~w",[Exec,URI]),
    shell(SyncCmd).

sync_cwd_to_remote(_Opts).

sync_remote_to_cwd(Opts) :-
    get_opt(sync,URI,Opts),
    !,
    sync_exec(Exec,URI,Opts),
    format(string(SyncCmd),"~w ~w .",[Exec,URI]),
    shell(SyncCmd).

sync_remote_to_cwd(_Opts).

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

rsync_exec("rsync").
