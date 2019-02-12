% * -*- Mode: Prolog -*- */

:- module(sync,
          [
	      sync_uri/1,
	      sync_cwd_to_remote/1,
	      sync_remote_to_cwd/1
          ]).

:- use_module(library(readutil)).
:- use_module(library(biomake/utils)).

:- discontiguous sync_uri/2.
:- discontiguous sync_cwd_to_uri/1.
:- discontiguous sync_uri_to_cwd/2.

% ----------------------------------------
% SYNC TO/FROM REMOTE STORAGE
% ----------------------------------------

sync_cwd_to_remote(Opts) :-
    get_opt(sync,URI,Opts),
    sync_cwd_to_uri(URI).

sync_remote_to_cwd(Opts) :-
    get_opt(sync,URI,Opts),
    sync_uri_to_cwd(URI).


% ----------------------------------------
% S3
% ----------------------------------------

sync_uri(URI) :-
    s3_sync_uri(URI,_,_).

s3_sync_uri(URI,Bucket,Prefix) :-
    format(string(URI),"s3://~w/~w",[Bucket,Prefix]).

s3_sync_exec("aws s3 sync --delete").

sync_cwd_to_uri(URI) :-
    s3_sync_uri(URI,_,_),
    s3_sync_exec(S3Exec),
    format(string(S3Cmd),"~w . ~w",[S3Exec,URI]),
    shell(QsubCmd).

sync_cwd_to_uri(URI) :-
    s3_sync_uri(URI,_,_),
    s3_sync_exec(S3Exec),
    format(string(S3Cmd),"~w ~w .",[S3Exec,URI]),
    shell(QsubCmd).
