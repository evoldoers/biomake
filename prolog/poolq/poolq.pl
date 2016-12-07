% * -*- Mode: Prolog -*- */

% Implements a simple job queue, with dependencies, using thread pools & message queues.

:- module(poolq,
          [
             poolq_create/3,
	     poolq_submit_job/5,
	     poolq_wait/2
          ]).

:- dynamic job_waiting/4.      % job_waiting(JobId,DepJobIds,Goal,Options).
:- dynamic job_running/2.      % job_running(JobId,Thread).
:- dynamic job_complete/2.     % job_complete(JobId,Status).

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
	debug(poolq,"Scheduling thread ~w terminated",[Scheduler]).

init_scheduler(Size,Options) :-
	debug(poolq,"Scheduler: initializing",[]),
	thread_self(Pool),
	thread_pool_create(Pool,Size,Options),
	debug(poolq,"Scheduler: created thread pool ~w with ~w threads, options ~w",[Pool,Size,Options]),
	wait_for_message.

wait_for_message :-
	receive_message(Msg),
	process_message(Msg).

receive_message(Msg) :-
	debug(poolq,"Scheduler: waiting for message",[]),
	thread_get_message(Msg),
	debug(poolq,"Scheduler: received message '~w'",[Msg]).

process_message(Msg) :-
	process_submit_message(Msg),
	!,
	wait_for_message.

process_message(Msg) :-
	process_complete_message(Msg),
	!,
	wait_for_message.

process_message(finish) :-
	!,
	finish_queued_jobs.

process_message(Msg) :-
	process_error(Msg),
	!,
	wait_for_message.

process_submit_message(submit(Goal,JobId,DepJobIds,Options)) :-
	none_waiting_or_running(DepJobIds),
	!,
	debug(poolq,"Scheduler: job ~w has no unmet dependencies, starting immediately",[JobId]),
	start_job(JobId,Goal,Options).

process_submit_message(submit(Goal,JobId,DepJobIds,Options)) :-
	!,
	debug(poolq,"Scheduler: job ~w has dependencies ~w; postponing",[JobId,DepJobIds]),
	assert(job_waiting(JobId,DepJobIds,Goal,Options)).

process_complete_message(complete(JobId,JobStatus)) :-
	job_running(JobId,Thread),
	!,
	debug(poolq,"Scheduler: job ~w on thread ~w finished with status ~w",[JobId,Thread,JobStatus]),
	retract(job_running(JobId,Thread)),
	assert(job_complete(JobId,JobStatus)),
	thread_join(Thread,ThreadStatus),
	debug(poolq,"Scheduler: job thread ~w terminated with status ~w",[Thread,ThreadStatus]),
	start_queued_jobs.

process_error(Msg) :-
	format("Error: couldn't process message '~w'~n",[Msg]).

finish_queued_jobs :-
	start_queued_jobs,
	wait_for_queue,
	thread_self(Pool),
	thread_pool_destroy(Pool),
	debug(poolq,"Scheduler: destroyed thread pool",[]).

wait_for_queue :-
	job_running(_,_),
	!,
	receive_message(Msg),
	(process_complete_message(Msg) ; process_error(Msg)),
	wait_for_queue.
wait_for_queue :-
	job_waiting(_,_,_,_),
	!,
	bagof(JobId,job_waiting(JobId,_,_,_),AbandonedJobs),
	format("Warning: unprocessed jobs ~w~n",[AbandonedJobs]),
	fail.
wait_for_queue.

start_queued_jobs :-
	debug(poolq,"Scheduler: looking for postponed jobs",[]),
	start_queued_job,
	!,
	start_queued_jobs.
start_queued_jobs :-
	\+ job_waiting(_,_,_,_),
	!,
	debug(poolq,"Scheduler: no jobs waiting",[]).
start_queued_jobs :-
	debug(poolq,"Scheduler: no jobs ready to run",[]).

start_queued_job :-
	job_waiting(JobId,DepJobIds,Goal,Options),
	none_waiting_or_running(DepJobIds),
	retract(job_waiting(JobId,DepJobIds,Goal,Options)),
	start_job(JobId,Goal,Options).

start_job(JobId,Goal,Options) :-
	debug(poolq,"Scheduler: starting job ~w: ~w",[Goal,Options]),
	thread_self(Self),
	thread_create_in_pool(Self,run_job(Self,JobId,Goal),Thread,Options),
	assert(job_running(JobId,Thread)).

run_job(Scheduler,JobId,Goal) :-
	job_status(Goal,Status),
	thread_send_message(Scheduler,complete(JobId,Status)),
	debug(poolq,"Job ~w: sent message 'complete(~w,~w)' to scheduling thread ~w",[JobId,JobId,Status,Scheduler]).

job_status(Goal,true) :- call(Goal), !.
job_status(_,false).

none_waiting_or_running(JobIds) :-
	forall(member(JobId,JobIds),
	       \+ (job_waiting(JobId,_,_,_)
                   ; job_running(JobId,_))).
