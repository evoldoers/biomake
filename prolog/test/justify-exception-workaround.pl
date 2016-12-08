% * -*- Mode: Prolog -*- */

:- use_module(library(thread_pool)).

% This is a minimal test case to justify bypassing prolog_exception_hook.
% The main program runs exception-free if sections A & B are both commented out.
% With section A commented out but section B present, three exceptions are reported.
% With sections A & B both present, no exceptions are reported.

:- dynamic prolog_exception_hook/4.

% Section A:
%user:prolog_exception_hook(error(existence_error(thread,_),context(system:thread_property/2,_)),_,_,_) :- !, fail.
%user:prolog_exception_hook('$aborted',_,_,_) :- !, fail.

% Section B:
user:prolog_exception_hook(E,_,_,_) :-
	format("Exception: ~w~n",[E]),
        backtrace(99),
        !,
        fail.

% Main program:
main :-
	thread_pool_create(my_pool,4,[]),
	thread_create_in_pool(my_pool,run_job(a,b,c),Thread,[]),
	thread_join(Thread,Status),
	format("Thread ~w finished with status ~w~n",[Thread,Status]).

run_job(X,Y,Z) :-
	format("X=~w Y=~w Z=~w~n",[X,Y,Z]).
