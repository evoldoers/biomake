% * -*- Mode: Prolog -*- */

:- module(md5hash,
          [
	      md5_hash_up_to_date/3,
	      ensure_md5_directory_exists/1,
	      update_md5_file/2
          ]).

:- use_module(library(readutil)).
:- use_module(library(biomake/utils)).

% ----------------------------------------
% MD5 HASHES
% ----------------------------------------

md5_prog("md5sum","").  % Ubuntu
md5_prog("md5","-q").  % MacOS, BSD

find_md5_prog(Exec) :-
	md5_prog(Prog,Args),
	find_on_path(Prog,Path),
	atomic_list_concat([Path,Args]," ",Exec),
	!.

:- dynamic md5_hash/3.
:- dynamic md5_valid/3.

md5_hash_up_to_date(T,DL,Opts) :-
    atom(T),
    !,
    atom_chars(T,Tc),
    string_chars(Tstr,Tc),
    md5_hash_up_to_date(Tstr,DL,Opts).
md5_hash_up_to_date(T,DL,_Opts) :-
    !,
    debug(md5,"Checking MD5 hash validity for ~w <-- ~w",[T,DL]),
    md5_check(T,S,H),
    md5_valid(T,S,H).

% read_md5_file attempts to find the MD5 hash of a target and (if known) the conditions under which this target is still valid.
% It does this by first trying to read the file .biomake/md5/{targetName}, then trying to lookup the hash in its database,
% and finally (if all else fails) computing the hash on-the-fly, using the md5 program.
read_md5_file(T) :-
    md5_filename(T,F),
    exists_file(F),
    !,
    debug(md5,'Reading MD5 hash file: ~w',[F]),
    retract_md5_hash(T),
    open(F,read,IO,[]),
    repeat,
    (   at_end_of_stream(IO)
     ->  !
     ;   read_term(IO,Term,[syntax_errors(error),
                            module(biomake)]),
         debug(md5,'parsed term: ~w',[Term]),
         assert(Term),
         fail),
    close(IO).

md5_check_size(File,Size,Hash) :- exists_file(File), size_file(File,Size), md5_hash(File,Size,Hash).

md5_check(File,Size,Hash) :- md5_check_size(File,Size,Hash), !.
md5_check(File,Size,Hash) :- read_md5_file(File), !, md5_check_size(File,Size,Hash).
md5_check(File,Size,Hash) :- compute_md5(File,Size,Hash).

retract_md5_hash(T) :-
    md5_hash(T,_,_),
    !,
    retractall(md5_hash(T,_,_));true.
retract_md5_hash(_).

compute_md5(T,Size,Hash) :-
    exists_file(T),
    size_file(T,Size),
    try_md5_prog(T,Hash),
    debug(md5,'MD5 hash of file ~w (size ~w) is ~w',[T,Size,Hash]),
    retract_md5_hash(T),
    assert(md5_hash(T,Size,Hash)).

% try the md5 executables findable with md5_prog
% use a temporary file instead of a pipe, since process_create doesn't seem to play well with threads in OSX :-(
try_md5_prog(Filename,Hash) :-
    find_md5_prog(Md5Prog),
    % the following is not infallible: if two targets with the same base name are built at the same time, we'll get a tempfile name clash
    % TODO: come up with a better scheme here
    file_base_name(Filename,Base),
    format(string(TmpPrefix),"md5_~w",[Base]),
    tmp_file(TmpPrefix,TmpFile),
    absolute_file_name(Filename,Path),
    format(string(Exec),"~w ~w >~w",[Md5Prog,Path,TmpFile]),
    debug(md5,'computing hash: ~w',[Exec]),
    shell(Exec),
    phrase_from_file(first_n(32,HashCodes),TmpFile),
    delete_file(TmpFile),
    string_codes(HashStr,HashCodes),
    debug(md5,'output of ~w ~w: ~w',[Md5Prog,Filename,HashStr]),
    string_lower(HashStr,Hash).

% fall back to using Prolog's in-memory MD5 implementation in the md5 library
%try_md5_prog(Filename,Hash) :-
%    use_module(library(md5), [ md5_hash/3 as library_md5_hash ]),
%    debug(md5,'reading ~w into memory for native SWI-Prolog MD5 implementation',[Filename]),
%    read_file_to_string(Filename,Str,[]),
%    library_md5_hash(Str,Hash,[]).

% fall back to using Prolog's deprecated in-memory MD5 implementation in the rdf_db library
try_md5_prog(Filename,Hash) :-
    use_module(library(semweb/rdf_db)),
    debug(md5,'reading ~w into memory for native SWI-Prolog MD5 implementation',[Filename]),
    read_file_to_string(Filename,Str,[]),
    rdf_atom_md5(Str,1,Hash).

first_n(0,[]) --> [].
first_n(0,[]) --> [_], first_n(0,[]).
first_n(N,[C|Cs]) --> [C], {Np is N - 1}, first_n(Np,Cs).

delete_md5_file(T) :-
    md5_filename(T,F),
    exists_file(F),
    !,
    delete_file(F).
delete_md5_file(_).

ensure_md5_directory_exists(Target) :-
    biomake_private_filename_dir_exists(Target,"md5",_).

md5_filename(Target,Filename) :-
    biomake_private_filename(Target,"md5",Filename).

open_md5_file(Target,Stream) :-
    open_biomake_private_file(Target,"md5",_,Stream).

make_md5_hash_term(T,S,H,Str) :-
    format(string(Str),"md5_hash(\"~w\",~d,\"~w\")",[T,S,H]).

make_md5_valid_term(T,S,H,Str) :-
    format(string(Str),"md5_valid(\"~w\",~d,\"~w\")",[T,S,H]).

make_md5_check_term(T,S,H,Str) :-
    format(string(Str),"md5_check(\"~w\",~d,\"~w\")",[T,S,H]).

make_md5_valid_goal_list([Dep|Deps],[Goal|Goals]) :-
    md5_check(Dep,Size,Hash),
    !,
    make_md5_check_term(Dep,Size,Hash,Goal),
    make_md5_valid_goal_list(Deps,Goals).
make_md5_valid_goal_list([_|Deps],Goals) :- make_md5_valid_goal_list(Deps,Goals), !.
make_md5_valid_goal_list([],[]).

update_md5_file(T,DL) :-
    debug(md5,'updating MD5 hash file for ~w <-- ~w',[T,DL]),
    delete_md5_file(T),
    md5_check(T,SizeT,HashT),
    make_md5_hash_term(T,SizeT,HashT,HashTerm),
    make_md5_valid_term(T,SizeT,HashT,ValidTerm),
    make_md5_valid_goal_list(DL,ValidGoals),
    open_md5_file(T,IO),
    format(IO,"~w.~n",[HashTerm]),
    debug(md5,' ~w',[HashTerm]),
    (ValidGoals = [] -> format(IO,"~w.~n",[ValidTerm]), debug(md5,' ~w.',[ValidTerm]);
     concat_string_list(ValidGoals,ValidGoalStr,",\n  "),
     format(IO,"~w :-~n  ~w.~n",[ValidTerm,ValidGoalStr]),
     debug(md5," ~w :-~n  ~w.",[ValidTerm,ValidGoalStr])),
    close(IO),
    !.

update_md5_file(T,DL) :-
    format("Warning: could not update MD5 file for ~w <-- ~w~n",[T,DL]),
    !.
