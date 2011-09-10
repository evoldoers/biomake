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
arg_info('--debug',atom,'(developers) debug target').

parse_arg(['--dry-run'|L],L,dry_run(true)).
parse_arg(['-n'|L],L,dry_run(true)).
arg_info('--dry-run','','Print the commands that would be executed, but do not execute them').

parse_arg(['-h'|L],L,null) :- show_help.

show_help :-
        Fake=[X|_],
        freeze(X,(show_help(X),fail)),
        parse_args(Fake,_).

show_help(X) :-
        format('~w',[X]),
        (   arg_info(X,Args,Info)
        ->  format(' ~w~n    ~w~n',[Args,Info])
        ;   nl).


