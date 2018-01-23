:- module(persistent_db,
          [
           attach_persistent_db/1
          ]).
:- use_module(library(persistency)).

:- persistent action(info:any, time:float, process_id:integer).

:- dynamic attached/0.

ensure_attached :-
        attached,
        !.
ensure_attached :-
        attach_persistent_db('.biomake-store.pro').

ensure_exists(File) :-
        exists_file(File),
        !.
ensure_exists(File) :-
        tell(File),
        told,
        !.


attach_persistent_db(File) :-
        ensure_exists(File),
        assert(attached),
        db_attach(File, []).

clear_persistent_db :-
        with_mutex(persistent_db,
                   retractall_action(_)).


:- multifile biomake:intercept_message_hook/1.
biomake:intercept_message_hook(M) :-
        ensure_attached,
        get_time(T),
        current_prolog_flag(pid, PID),
        assert_action(M,T,PID).

