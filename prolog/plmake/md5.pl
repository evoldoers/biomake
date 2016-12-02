% * -*- Mode: Prolog -*- */

:- module(md5,
          [
	      md5_hash_up_to_date/3,
	      update_md5_file/2
          ]).


% ----------------------------------------
% MD5 HASHES
% ----------------------------------------

:- dynamic md5_hash/2.
:- dynamic md5_valid/2.

md5_hash_up_to_date(_,[],_) :- !.
md5_hash_up_to_date(T,DL,Opts) :-
    atom(T),
    !,
    atom_chars(T,Tc),
    string_chars(Ts,Tc),
    md5_hash_up_to_date(Ts,DL,Opts).
md5_hash_up_to_date(T,DL,_Opts) :-
    !,
    debug(md5,"Checking MD5 hash validity for ~w <-- ~w",[T,DL]),
    read_md5_file(T),
    md5_hash(T,H),
    md5_valid(T,H).

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
                            module(plmake)]),
         debug(md5,'parsed term: ~w',[Term]),
         assert(Term),
         fail),
    close(IO).

read_md5_file(T) :-
    md5_check(T,_).

md5_check(T,Hash) :- md5_hash(T,Hash), !.
md5_check(T,Hash) :- compute_md5(T,Hash).

retract_md5_hash(T) :-
    md5_hash(T,_),
    !,
    retractall(md5_hash(T,_));true.
retract_md5_hash(_).

compute_md5(T,Hash) :-
    exists_file(T),
    format(string(Exec),"md5 -q ~w",[T]),
    debug(md5,'computing hash: ~w',[Exec]),
    shell_eval(Exec,HashCodes),
    phrase(chomp(Hash),HashCodes),
    debug(md5,'MD5 hash of ~w is ~w',[T,Hash]),
    retract_md5_hash(T),
    assert(md5_hash(T,Hash)).

chomp(S) --> string_from_codes(S,"\n"), [10].

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

make_md5_hash_term(T,H,S) :-
    format(string(S),"md5_hash(\"~w\",~q)",[T,H]).

make_md5_valid_term(T,H,S) :-
    format(string(S),"md5_valid(\"~w\",~q)",[T,H]).

make_md5_check_term(T,H,S) :-
    format(string(S),"md5_check(\"~w\",~q)",[T,H]).

make_md5_valid_goal_list([Dep|Deps],[Goal|Goals]) :-
    md5_check(Dep,Hash),
    !,
    make_md5_check_term(Dep,Hash,Goal),
    make_md5_valid_goal_list(Deps,Goals).
make_md5_valid_goal_list([_|Deps],Goals) :- make_md5_valid_goal_list(Deps,Goals), !.
make_md5_valid_goal_list([],[]).

% this next bit is a little messy: I think there is a more elegant way of constructing the terms
% and then writing them to the file, rather than doing a formatted write
update_md5_file(T,DL) :-
    debug(md5,'updating MD5 hash file for ~w <-- ~w',[T,DL]),
    delete_md5_file(T),
    md5_check(T,HashT),
    make_md5_hash_term(T,HashT,HashTerm),
    make_md5_valid_term(T,HashT,ValidTerm),
    make_md5_valid_goal_list(DL,ValidGoals),
    md5_filename_mkdir(T,F),
    open(F,write,IO,[]),
    format(IO,"~w.~n",[HashTerm]),
    debug(md5,' ~w',[HashTerm]),
    (ValidGoals = [] -> format(IO,"~w.~n",[ValidTerm]), debug(md5,' ~w.',[ValidTerm]);
     concat_string_list(ValidGoals,ValidGoalStr,",~n  "),
     format(IO,"~w :-~n  ~w.~n",[ValidTerm,ValidGoalStr]),
     debug(md5," ~w :-~n  ~w.",[ValidTerm,ValidGoalStr])),
    close(IO),
    !.

update_md5_file(T,DL) :-
    format("Warning: could not update MD5 file for ~w <-- ~w~n",[T,DL]),
    !.
