% * -*- Mode: Prolog -*- */

:- module(scan,
          [
           show_stored_targets/0,
           collect_stored_targets/1,
           collect_stored_targets/2,

           stored_target/5
           ]).
:- use_module(biomake).

% ----------------------------------------
% DATABASE
% ----------------------------------------

%% stored_target(?Target, ?Dependencies, ?Status, ?TimeBuilt, ?TimeChecked)
:- dynamic stored_target/5.

%% collect_stored_targets(+Path,+Opts:list)
%
% iterates over files and directories in Path, generating metadata
% on each file, storing in stored_target/5
collect_stored_targets(Opts):-
        collect_stored_targets('.',Opts).
collect_stored_targets(Path,Opts):-
        expand_file_name(Path,Dirs),
        forall(member(Dir,Dirs),
               (   dir_files_recursive(Dir,Files,[]),
                   forall(member(File,Files),
                          collect_file(File,Opts)))).

dir_files_recursive(Dir,AllFiles,VL) :-
        directory_files(Dir,Files),
        findall(FilePath,(member(File,Files),
                          atomic_list_concat([Dir,/,File],FilePath)),
                FilePaths),
        debug(biomake,'dir files: ~w',[Dir]),
        findall(FilePath,
                (   member(Dir2,Files),
                    Dir2\='.',
                    Dir2\='..',
                    \+ member(Dir2,VL),
                    atomic_list_concat([Dir,/,Dir2],Dir3),
                    exists_directory(Dir3),
                    dir_files_recursive(Dir3,Files,[Dir2|VL]),
                    member(File,Files),
                    atomic_list_concat([Dir3,/,File],FilePath)
                    ),
                Files2),
        append(FilePaths,Files2,AllFiles).

collect_file(File,Opts) :-
        debug(biomake,'collecting: "~w"',[File]),
        get_time(TimeChecked),
        file_info(File,DL,Status,Opts),
        debug(biomake,'  info: ~w',[Status]),
        time_file_wrap(File,TimeBuilt),
        retractall(stored_target(File,_,_,_,_)),
        assert(stored_target(File,DL,Status,TimeBuilt,TimeChecked)).

file_info(T,DL,Status,Opts) :-
        target_bindrule(T,Rule),
        rule_dependencies(Rule,DL,Opts),
        (   rebuild_required(T,DL,[],Opts)
        ->  Status=stale
        ;   Status=fresh),
        !.
file_info(_,[],unknown,_).

time_file_wrap(F,T) :-
        exists_file(F),
        time_file(F,T),
        !.
time_file_wrap(_,-).

show_stored_targets :-
        forall(stored_target(T,DL,S,T1,T2),
	       (atomic_list_concat(DL,' ',DLA),
	        format("~w [~w] ~w ~w ~w~n",[T,DLA,S,T1,T2]))).
