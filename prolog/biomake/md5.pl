% * -*- Mode: Prolog -*- */

:- module(md5,
          [
	      md5_hash_up_to_date/3,
	      update_md5_file/2
          ]).

:- use_module(library(md5), [ md5/2 as library_md5 ]).

% ----------------------------------------
% MD5 HASHES
% ----------------------------------------

md5_prog("md5sum").  % Ubuntu
md5_prog("md5 -q").  % MacOS, BSD

:- dynamic md5_hash/3.
:- dynamic md5_valid/3.

md5_hash_up_to_date(T,DL,Opts) :-
    atom(T),
    !,
    atom_chars(T,Tc),
    string_chars(Ts,Tc),
    md5_hash_up_to_date(Ts,DL,Opts).
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

try_md5_prog(Filename,Hash) :-
    md5_prog(Md5Prog),
    format(string(Exec),"~w ~w",[Md5Prog,Filename]),
    debug(md5,'computing hash: ~w',[Exec]),
    shell_eval(Exec,ExecOut),
    phrase(first_n(32,HashCodes),ExecOut),
    string_codes(HashStr,HashCodes),
    string_lower(HashStr,Hash).

first_n(0,[]) --> [].
first_n(0,[]) --> [_], first_n(0,[]).
first_n(N,[C|Cs]) --> [C], {Np is N - 1}, first_n(Np,Cs).

delete_md5_file(T) :-
    md5_filename(T,F),
    exists_file(F),
    !,
    delete_file(F).
delete_md5_file(_).

md5_filename(Target,Filename) :-
    absolute_file_name(Target,F),
    file_directory_name(F,D),
    file_base_name(F,N),
    format(string(Filename),"~w/.biomake/md5/~w",[D,N]).

md5_filename_mkdir(Target,Filename) :-
    md5_filename(Target,Filename),
    file_directory_name(Filename,D),
    format(string(MkDir),"mkdir -p ~w",[D]),
    shell(MkDir).

make_md5_hash_term(T,S,H,Str) :-
    format(string(Str),"md5_hash(\"~w\",~d,~q)",[T,S,H]).

make_md5_valid_term(T,S,H,Str) :-
    format(string(Str),"md5_valid(\"~w\",~d,~q)",[T,S,H]).

make_md5_check_term(T,S,H,Str) :-
    format(string(Str),"md5_check(\"~w\",~d,~q)",[T,S,H]).

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
    md5_filename_mkdir(T,F),
    open(F,write,IO,[]),
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
