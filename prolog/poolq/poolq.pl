% * -*- Mode: Prolog -*- */

% Implements a simple job queue, with dependencies, using thread pools & message queues.

:- module(poolq,
          [
             poolq_create/3,
	     poolq_submit_job/5,
	     poolq_wait/2
          ]).

:- dynamic job_in_queue/5.     % job_in_queue(Pool,JobId,DepJobIds,Goal,Options).
:- dynamic job_running/3.      % job_running(Pool,JobId,Thread).
:- dynamic job_complete/3.     % job_complete(Pool,JobId,Status).

:- use_module(library(thread_pool)).

poolq_create(Scheduler,Size,Options) :-
	thread_create(init_scheduler(Size,Options),Scheduler,[]),
	debug(poolq,"Started scheduling thread ~w",[Scheduler]).

poolq_submit_job(Scheduler,Goal,JobId,JobDepIds,Options) :-
	thread_send_message(Scheduler,submit(Goal,JobId,JobDepIds,Options)),
	debug(poolq,"Sent message 'submit(~w,~w <-- ~w,~w)' to scheduling thread ~w",[Goal,JobId,JobDepIds,Options,Scheduler]).

poolq_wait(Scheduler,Status) :-
	thread_send_message(Scheduler,finish),
	debug(poolq,"Sent message 'finish' to scheduling thread ~w",[Scheduler]),
	thread_join(Scheduler,Status),
	debug(poolq,"Scheduling thread ~w joined",[Scheduler]).

init_scheduler(Size,Options) :-
	debug(poolq,"Starting scheduler",[]),
	thread_pool_create(Pool,Size,Options),
	debug(poolq,"Created thread pool ~w with ~w threads, options ~w",[Pool,Size,Options]),
	wait_for_message(Pool).

wait_for_message(Pool) :-
	thread_get_message(Msg),
	debug(poolq,"Received message ~w for thread pool ~w",[Msg,Pool]),
	process_message(Pool,Msg).

process_message(Pool,Msg) :-
	process_submit_message(Pool,Msg),
	!,
	wait_for_message(Pool).

process_message(Pool,Msg) :-
	process_complete_message(Pool,Msg),
	!,
	wait_for_message(Pool).

process_message(Pool,finish) :-
	!,
	finish_queued_jobs(Pool).

process_message(Pool,Msg) :-
	process_error(Msg),
	!,
	wait_for_message(Pool).

process_submit_message(Pool,submit(Goal,JobId,DepJobIds,Options)) :-
	none_pending(Pool,DepJobIds),
	!,
	start_job(Pool,JobId,Goal,Options).

process_submit_message(Pool,submit(Goal,JobId,DepJobIds,Options)) :-
	!,
	assert(job_in_queue(Pool,JobId,DepJobIds,Goal,Options)).

process_complete_message(Pool,complete(JobId,JobStatus)) :-
	job_running(Pool,JobId,Thread),
	retract(job_running(Pool,JobId,Thread)),
	assert(job_complete(Pool,JobId,JobStatus)),
	thread_join(Thread,_ThreadStatus),
	start_queued_jobs(Pool).

process_error(Msg) :-
	format("Error: couldn't process message ~w~n",[Msg]).

finish_queued_jobs(Pool) :-
	start_queued_jobs(Pool),
	wait_for_queue(Pool).

wait_for_queue(Pool) :-
	job_running(Pool,_,_),
	!,
	thread_get_message(Msg),
	(process_complete_message(Pool,Msg) -> process_error(Msg)),
	wait_for_queue(Pool).
wait_for_queue(Pool) :-
	job_in_queue(Pool,_,_,_,_),
	!,
	bagof(JobId,job_in_queue(Pool,JobId,_,_,_),AbandonedJobs),
	format("Warning: unprocessed jobs ~w~n",[AbandonedJobs]),
	fail.
wait_for_queue(_).

start_queued_jobs(Pool) :-
	start_queued_job(Pool),
	!,
	start_queued_jobs(Pool).
start_queued_jobs(_).

start_queued_job(Pool) :-
	job_in_queue(Pool,JobId,DepJobIds,Goal,Options),
	none_pending(DepJobIds),
	retract(job_in_queue(Pool,JobId,DepJobIds,Goal,Options)),
	start_job(Pool,JobId,Goal,Options).

start_job(Pool,JobId,Goal,Options) :-
	thread_self(Self),
	assert(job_running(Pool,JobId,Thread)),
	thread_create_in_pool(Pool,run_job(Self,JobId,Goal),Thread,Options).

run_job(Scheduler,JobId,Goal) :-
	job_status(Goal,Status),
	thread_send_message(Scheduler,complete(JobId,Status)).

job_status(Goal,true) :- call(Goal), !.
job_status(_,false).

none_pending(Pool,JobIds) :-
	forall(member(JobId,JobIds),not_pending(Pool,JobId)).
not_pending(Pool,JobId) :- \+ pending(Pool,JobId).

pending(Pool,JobId) :- job_in_queue(Pool,JobId,_,_), !.
pending(Pool,JobId) :- job_running(Pool,JobId,_), !.
