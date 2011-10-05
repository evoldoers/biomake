% * -*- Mode: Prolog -*- */

:- use_module(library(plmake/plmake)).

user:prolog_exception_hook(_,
                           _, _, _) :-
        backtrace(99),
        !,
        fail.

main :-
        current_prolog_flag(argv, Arguments),
        append(_SytemArgs, [--|Args], Arguments),
        !,
        parse_args(Args,Opts_1),
        flatten(Opts_1,Opts),
        consult_buildfile,
        forall(member(goal(G),Opts),
               G),
        forall(member(rest(T),Opts),
               build(T,Opts)),
        halt.

% ----------------------------------------
% OPTION PROCESSING
% ----------------------------------------

parse_args([],[]).
parse_args(Args,[Opt|Opts]) :-
        parse_arg(Args,Rest,Opt),
        !,
        parse_args(Rest,Opts).
parse_args([A|Args],[rest(A)|Opts]) :-
        parse_args(Args,Opts).

:- discontiguous parse_arg/3.
:- discontiguous arg_info/3.

parse_arg(['--debug',D|L],L,null) :- debug(D), set_prolog_flag(verbose,normal).
arg_info('--debug','TARGET','[developers] debug target. E.g. --debug plmake').

parse_arg(['--dry-run'|L],L,dry_run(true)).
parse_arg(['-n'|L],L,dry_run(true)).
arg_info('--dry-run','','Print the commands that would be executed, but do not execute them').
arg_info('-n','','Shortcut for --dry-run').

parse_arg(['-h'|L],L,null) :-
        show_help,
        nl,
        writeln('For more info see http://github.com/cmungall/plmake'),
        nl,
        halt.
arg_info('-h','','Show help').


parse_arg(['--always-make'|L],L,always_make(true)).
arg_info('--always-make','','Always build fresh target even if dependency is up to date').

parse_arg(['-t',F|L],L,null) :-
        !,
        ensure_loaded(library(plmake/gnumake_parser)),
        translate_makefile(F).
arg_info('-t','MAKEFILE','Translates a GNU Makefile to a makeprog [incomplete]').

parse_arg(['-l',F|L],L,
          goal( (collect_stored_targets(F,[]),
                 show_stored_targets
                ) )) :-
        ensure_loaded(library(plmake/plmake_db)),
        !.
arg_info('-l','DIRECTORY','Iterates through directory writing metadata on each file found').
        




show_help :-
        Fake=[X|_],
        freeze(X,(show_help(X),fail)),
        parse_args(Fake,_).

show_help(X) :-
        format('~w',[X]),
        (   arg_info(X,Args,Info)
        ->  format(' ~w~n    ~w~n',[Args,Info])
        ;   nl).


