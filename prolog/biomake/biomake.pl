% * -*- Mode: Prolog -*- */

:- module(biomake,
          [
	   disable_backtrace/0,
	   call_without_backtrace/1,
	      
	   build_default/0,
           build_default/1,

	   halt_success/0,
	   halt_error/0,

	   bind_special_variables/1,
	   start_queue/1,
           build/1,
           build/2,
           build/3,
	   finish_queue/1,

	   report/3,
	   report/4,

	   verbose_report/3,
	   verbose_report/4,

	   consult_gnu_makefile/3,
           consult_makeprog/3,
	   read_makeprog_stream/4,
	   
	   read_string_as_makeprog_term/3,
	   read_atom_as_makeprog_term/3,
	   eval_atom_as_makeprog_term/3,

	   add_spec_clause/3,
	   add_spec_clause/4,
	   add_cmdline_assignment/1,
	   add_gnumake_clause/3,
	   
	   global_binding/2,
	   
           target_bindrule/3,
           rebuild_required/4,

	   normalize_pattern/3,
	   unwrap_t/2,

           rule_target/3,
           rule_dependencies/3,
           rule_execs/3,

	   run_execs_now/3,
	   report_run_exec/3,
	   update_hash/3,

	   bindvar/3,
	   expand_vars/2,
	   expand_vars/3
           ]).

:- use_module(library(biomake/utils)).
:- use_module(library(biomake/functions)).
:- use_module(library(biomake/embed)).

:- user:op(1100,xfy,<--).
:- user:op(1101,xfy,?=).
:- user:op(1102,xfy,:=).
:- user:op(1103,xfy,+=).
:- user:op(1104,xfy,=*).

% Configuration
max_recurse_depth(100).

/** <module> Prolog implementation of Makefile-inspired build system

  See the README

  */

% ----------------------------------------
% EXCEPTIONS
% ----------------------------------------

% use disable_backtrace to permanently disable backtrace on exception,
% and call_without_backtrace to temporarily disable it.
:- dynamic no_backtrace/0.
:- dynamic suppress_backtrace/0.

% Intercept a couple of exceptions that are thrown by the threadpool library
% This is kind of yucky, but only seems to affect our exception-handling code
:- dynamic user:prolog_exception_hook/4.

user:prolog_exception_hook(error(existence_error(thread,_),context(system:thread_property/2,_)),_,_,_) :- !, fail.
user:prolog_exception_hook('$aborted',_,_,_) :- !, fail.

% Default exception handler: show backtrace
user:prolog_exception_hook(E,_,_,_) :-
	format("Exception: ~w~n",[E]),
        (no_backtrace; (suppress_backtrace; backtrace(99))),
        !,
        fail.

call_without_backtrace(Term) :-
        assertz(suppress_backtrace),
	catch(call(Term),_,fail),
	retract(suppress_backtrace).

disable_backtrace :- assertz(no_backtrace).


% ----------------------------------------
% TOP-LEVEL
% ----------------------------------------

%% build_default()
%% build_default(+Opts:list)

%% build(+Target)
%% build(+Target, +Opts:list)
%% build(+Target, +Stack:list,+Opts:list)
%
% builds Target using rules
% Call start_queue/1 beforehand and finish_queue/1 afterwards.

build_default :-
	build_default([]).

build_default(Opts) :-
        default_target(T),
	!,
	build(T,Opts).

build_default(_) :-
	format("No targets. Stop.~n").

build :-
	format("No targets. Stop.~n").

build(T) :-
        build(T,[]).
build(T,Opts) :-
        build(T,[],Opts).

build(T,SL,Opts) :-
	cyclic_dependency(T,SL,Opts),
	!,
        halt_error.

build(_T,SL,Opts) :-
	recursion_too_deep(SL,Opts),
	!,
	fail.

build(T,SL,Opts) :-
        debug_report(build,'Target: ~w',[T],SL),
        target_bindrule(T,Rule,Opts),   % match target name, test target goal
        debug_report(build,'Rule: ~w',[Rule],SL),
        rule_dependencies(Rule,DL,Opts),
        debug_report(build,'Dependencies: ~w',[DL],SL),
        can_build_deps(DL,[T|SL],Opts),  % test theoretical path to dependencies
        debug_report(build,'Dependencies buildable: ~w',[DL],SL),
        verbose_report('Checking dependencies: ~w <-- ~w',[T,DL],[T|SL],Opts),
        build_deps(DL,[T|SL],Opts),  % build dependencies
        debug_report(build,'Dependencies built: ~w',[DL],SL),
	dep_bindrule(Rule,Opts,Rule2,Opts2),  % test dependencies goal
        debug_report(build,'Post-dependency rule: ~w',[Rule2],SL),
        (   rebuild_required(T,DL,SL,Opts2)  % test if target is stale
        ->  run_execs_and_update(Rule2,SL,Opts2)  % (re)build
        ;   verbose_report('~w is up to date',[T],SL,Opts)),
	!.
build(T,SL,Opts) :-
        exists_file(T),
	!,
        verbose_report('Nothing to be done for ~w',[T],SL,Opts).
build(T,SL,Opts) :-
        \+ target_bindrule(T,_,Opts),
	handle_error('Don\'t know how to make ~w',[T],SL,Opts),
	!.
build(T,SL,Opts) :-
        handle_error('~w FAILED',[T],SL,Opts).

% tests of pathological conditions
cyclic_dependency(T,SL,Opts) :-
	member(Dep,SL),
	equal_as_strings(Dep,T),
	reverse(SL,SLrev),
	concat_string_list(SLrev,Chain," <-- "),
	report("Cyclic dependency detected: ~w <-- ~w",[Chain,T],SL,Opts).

recursion_too_deep(SL,Opts) :-
	length(SL,Depth),
	max_recurse_depth(D),
	Depth > D,
	report("Exceeds maximum length of dependency chain (~w)",[D],SL,Opts).

% test whether theoretical path exists
can_build_deps(_,_,Opts) :- get_opt(no_deps,true,Opts), !.
can_build_deps([],_,_).
can_build_deps([T|TL],SL,Opts) :-
        can_build_dep(T,SL,Opts),
        can_build_deps(TL,SL,Opts).

can_build_dep(T,SL,_) :-
        debug_report(build,'Checking theoretical path to ~w',T,SL),
	fail.
can_build_dep(T,SL,Opts) :-
	cyclic_dependency(T,SL,Opts),
	!,
        fail.
can_build_dep(_,SL,Opts) :-
	recursion_too_deep(SL,Opts),
	!,
        fail.
can_build_dep(T,_,_) :-
	\+ target_bindrule_exact(T),
	exists_file(T),
	!.
can_build_dep(T,SL,Opts) :-
        target_bindrule(T,Rule,Opts),
        rule_dependencies(Rule,DL,Opts),
        can_build_deps(DL,[T|SL],Opts).

% recursive build dependencies
build_deps(_,_,Opts) :- get_opt(no_deps,true,Opts), !.
build_deps([],_,_).
build_deps([T|TL],SL,Opts) :-
        build(T,SL,Opts),
        build_deps(TL,SL,Opts).

% Special vars
bind_special_variables(Opts) :-
        get_opt(biomake_prog,Prog,Opts),
	add_spec_clause(('MAKE' = Prog),[],[]),
	bagof(Arg,member(biomake_args(Arg),Opts),Args),
	atomic_list_concat(Args,' ',ArgStr),
	add_spec_clause(('MAKEFLAGS' = ArgStr),[],[]).

% Queue setup/wrapup
start_queue(Opts) :-
	get_opt(queue,Q,Opts),
	!,
	ensure_loaded(library(biomake/queue)),
	init_queue(Q,Opts).
start_queue(_).

finish_queue(Opts) :-
	get_opt(queue,Q,Opts),
	!,
	release_queue(Q).
finish_queue(_).


% ----------------------------------------
% REPORTING
% ----------------------------------------

report(Fmt,Args,Opts) :-
        report(Fmt,Args,[],Opts).

report(Fmt,Args,SL,_) :-
        stack_indent(SL,Fmt,IndentedFmt),
        format(IndentedFmt,Args),
        nl.

verbose_report(Fmt,Args,Opts) :- verbose_report(Fmt,Args,[],Opts).
verbose_report(Fmt,Args,[],_Opts) :-
        report(Fmt,Args,[]),
        !.
verbose_report(Fmt,Args,SL,_Opts) :-
	debug_report(verbose,Fmt,Args,SL).

stack_indent([],Text,Text).
stack_indent([_|T],Text,Indented) :-
        string_concat(' ',Text,Tab),
	stack_indent(T,Tab,Indented).

debug_report(Topic,Fmt,Args) :-
        debug_report(Topic,Fmt,Args,[]).

debug_report(Topic,Fmt,Args,SL) :-
        stack_indent(SL,Fmt,IndentedFmt),
	debug(Topic,IndentedFmt,Args).

% ----------------------------------------
% DEPENDENCY MANAGEMENT
% ----------------------------------------

% The interactions between the various options are a little tricky...
% Essentially (simplifying a little): MD5 overrides timestamps, except when queues are used.
rebuild_required(T,DL,SL,Opts) :-
	member(what_if(D),Opts),
        member(D,DL),
        !,
        verbose_report('Target ~w has dependency ~w marked as modified from the command-line - build required',[T,D],SL,Opts).
rebuild_required(T,_,SL,Opts) :-
        atom_string(T,Ts),
        member(old_file(Ts),Opts),
        !,
        verbose_report('Target ~w marked as old from the command-line - no rebuild required',[T],SL,Opts),
	fail.
rebuild_required(T,_,SL,Opts) :-
        \+ exists_target(T,Opts),
        !,
        verbose_report('Target ~w not materialized - build required',[T],SL,Opts).
rebuild_required(T,DL,SL,Opts) :-
        (get_opt(dry_run,true,Opts)
         ; \+ get_opt(md5,true,Opts)),
	has_rebuilt_dependency(T,DL,D,Opts),
	!,
        verbose_report('Target ~w has rebuilt dependency ~w - rebuild required',[T,D],SL,Opts).
rebuild_required(T,DL,SL,Opts) :-
	building_asynchronously(Opts),
	has_rebuilt_dependency(T,DL,D,Opts),
	!,
        verbose_report('Target ~w has dependency ~w on rebuild queue - rebuild required',[T,D],SL,Opts).
rebuild_required(T,DL,SL,Opts) :-
        member(D,DL),
        \+ exists_target(D,Opts),
	\+ member(old_file(D),Opts),
        !,
        verbose_report('Target ~w has unbuilt dependency ~w - rebuild required',[T,D],SL,Opts).
rebuild_required(T,DL,SL,Opts) :-
        \+ get_opt(md5,true,Opts),
	has_newer_dependency(T,DL,D,Opts),
	!,
        verbose_report('Target ~w built before dependency ~w - rebuild required',[T,D],SL,Opts).
rebuild_required(T,DL,SL,Opts) :-
        get_opt(md5,true,Opts),
	\+ md5_hash_up_to_date(T,DL,Opts),
	!,
        verbose_report('Target ~w does not have an up-to-date checksum - rebuild required',[T],SL,Opts).
rebuild_required(T,_,SL,Opts) :-
        get_opt(always_make,true,Opts),
        \+ build_count(T,_),  % don't always_make the same target twice
        !,
        verbose_report('Specified --always-make; rebuild required for target ~w',[T],SL,Opts).

building_asynchronously(Opts) :-
	get_opt(queue,Q,Opts),
	Q \= 'test'.

has_newer_dependency(T,DL,D,Opts) :-
        member(D,DL),
	\+ member(old_file(D),Opts),
        has_newer_timestamp(D,T,Opts).

has_rebuilt_dependency(T,DL,D,Opts) :-
        member(D,DL),
	\+ member(old_file(D),Opts),
	was_built_after(D,T,Opts).

rebuild_required_by_time_stamp(T,DL,SL,Opts) :-
        member(D,DL),
	was_built_after(D,T,Opts),
	!,
        verbose_report('Target ~w has rebuilt dependency ~w - rebuilding',[T,D],SL,Opts).
rebuild_required_by_time_stamp(T,DL,SL,Opts) :-
        \+ exists_directory(T),
        member(D,DL),
        has_newer_timestamp(D,T,Opts),
        !,
        verbose_report('Target ~w built before dependency ~w - rebuilding',[T,D],SL,Opts).

has_newer_timestamp(A,B,_Opts) :-
        time_file(A,TA),
        time_file(B,TB),
        TA > TB.

was_built_after(D,T,_Opts) :-
        build_count(D,Nd),
        (build_count(T,Nt) -> Nd > Nt; true).

exists_target(T,_Opts) :-
        exists_file(T).
exists_target(T,_Opts) :-
        exists_directory(T).

rule_target(rb(T,_,_,_,_),T,_Opts).
rule_dependencies(rb(_,DL,_,_,_),DL,_Opts).
rule_dep_goal(rb(_,_,DepGoal,_,_),DepGoal,_Opts).
rule_execs(rb(_,_,_,X,_),X,_Opts) :- !.
rule_execs(rb(_,_,_,X,_),_,_Opts) :- throw(error(no_exec(X))).
rule_vars(rb(_,_,_,_,V),V,_Opts).


% internal tracking of build order
% a bit hacky to use global assertions/retractions for this
:- dynamic build_count/2.
:- dynamic build_counter/1.

flag_as_rebuilt(T) :-
    next_build_counter(N),
    retractall(build_count(T,_)),
    assertz(build_count(T,N)).

next_build_counter(N) :-
    build_counter(Last),
    !,
    N is Last + 1,
    retract(build_counter(Last)),
    assertz(build_counter(N)).

next_build_counter(1) :-
    assertz(build_counter(1)).


% ----------------------------------------
% TASK EXECUTION
% ----------------------------------------

run_execs_and_update(Rule,SL,Opts) :-
    get_opt(dry_run,true,Opts),
    !,
    rule_target(Rule,T,Opts),
    rule_execs(Rule,Execs,Opts),
    forall(member(Exec,Execs),
	   (string_chars(Exec,['@'|EC])
	    -> (string_chars(ES,EC),
		report('~w',[ES],SL,Opts))
	    ; report('~w',[Exec],SL,Opts))),
    flag_as_rebuilt(T).

run_execs_and_update(Rule,SL,Opts) :-
    rule_target(Rule,T,Opts),
    dispatch_run_execs(Rule,SL,Opts),
    flag_as_rebuilt(T).

dispatch_run_execs(Rule,SL,Opts) :-
        get_opt(touch_only,true,Opts),
	!,
        rule_target(Rule,T,Opts),
        rule_dependencies(Rule,DL,Opts),
	format(string(Cmd),"touch ~w",[T]),
	shell(Cmd),
	(running_silent(Opts) -> true; report('~w',[Cmd],SL,Opts)),
	update_hash(T,DL,Opts).
dispatch_run_execs(Rule,SL,Opts) :-
	get_opt(queue,Q,Opts),
	!,
	rule_target(Rule,T,Opts),
	(get_opt(md5,true,Opts) -> ensure_md5_directory_exists(T) ; true),
	run_execs_in_queue(Q,Rule,SL,Opts),
	verbose_report('~w queued for rebuild',[T],SL,Opts).
dispatch_run_execs(Rule,SL,Opts) :-
	run_execs_now(Rule,SL,Opts),
	rule_target(Rule,T,Opts),
	verbose_report('~w built',[T],SL,Opts).

run_execs_now(Rule,SL,Opts) :-
	get_opt(oneshell,true,Opts),
	!,
	run_execs_in_script(Rule,SL,Opts).
run_execs_now(Rule,SL,Opts) :-
	rule_target(Rule,T,Opts),
        rule_dependencies(Rule,DL,Opts),
	rule_execs(Rule,Es,Opts),
	run_execs(Es,SL,Opts),
	update_hash(T,DL,Opts).

run_execs_in_script(Rule,SL,Opts) :-
        ensure_loaded(library(biomake/queue)),
        rule_target(Rule,T,Opts),
        rule_dependencies(Rule,DL,Opts),
	rule_execs(Rule,Es,Opts),
	write_script_file(T,Es,Opts,Script),
	report_run_exec(Script,SL,Opts),
	update_hash(T,DL,Opts).

update_hash(T,DL,Opts) :-
    get_opt(md5,true,Opts),
    !,
    update_md5_file(T,DL,Opts).
update_hash(_,_,_).

run_execs([],_,_).
run_execs([E|Es],SL,Opts) :-
        run_exec(E,SL,Opts),
        run_execs(Es,SL,Opts).

run_exec(Exec,SL,Opts) :-
	string_chars(Exec,['@'|SilentChars]),
	!,
	string_chars(Silent,SilentChars),
	silent_run_exec(Silent,SL,Opts).
run_exec(Exec,SL,Opts) :-
        running_silent(Opts),
        silent_run_exec(Exec,SL,Opts).
run_exec(Exec,SL,Opts) :-
	report_run_exec(Exec,SL,Opts).

report_run_exec(Exec,SL,Opts) :-
        report('~w',[Exec],SL,Opts),
	silent_run_exec(Exec,SL,Opts).

running_silent(Opts) :-
        get_opt(silent,true,Opts),
        \+ get_opt(dry_run,true,Opts).

silent_run_exec(Exec,SL,Opts) :-
        get_time(T1),
        shell(Exec,Err),
        get_time(T2),
        DT is T2-T1,
        debug_report(build,'  Return: ~w Time: ~w',[Err,DT],SL),
	handle_exec_error(Exec,Err,SL,Opts),
        !.

handle_exec_error(_,0,_,_) :- !.
handle_exec_error(Exec,Err,SL,Opts) :-
        handle_error('Error ~w executing ~w',[Err,Exec],SL,Opts).

handle_error(Fmt,Args,SL,Opts) :-
        report(Fmt,Args,SL),
	handle_error(Opts).

handle_error(Opts) :-
        get_opt(keep_going_on_error,true,Opts),
        \+ get_opt(stop_on_error,true,Opts),
        !.
handle_error(_) :-
        halt_error.


% ----------------------------------------
% READING AND WRITING MAKEPROGS
% ----------------------------------------

:- dynamic global_cmdline_binding/2.
:- dynamic global_simple_binding/2.
:- dynamic global_lazy_binding/2.

:- dynamic default_target/1.

is_assignment_op(=).
is_assignment_op(?=).
is_assignment_op(:=).
is_assignment_op(+=).
is_assignment_op(=*).

consult_makeprog(F,AllOpts,Opts) :-
        debug(makeprog,'reading: ~w',[F]),
        open(F,read,IO,[]),
	read_makeprog_stream(IO,AllOpts,Opts,_),
        debug(makeprog,'read: ~w',[F]).

consult_gnu_makefile(F,AllOpts,Opts) :-
        ensure_loaded(library(biomake/gnumake_parser)),
        parse_gnu_makefile(F,M,AllOpts,Opts),
	(member(translate_gnu_makefile(P),AllOpts)
	 -> translate_gnu_makefile(M,P); true).

read_makeprog_stream(IO,Opts,Opts,[]) :-
        at_end_of_stream(IO),
	!,
	close(IO).

read_makeprog_stream(IO,OptsOut,OptsIn,Terms) :-
        read_term(IO,Term,[variable_names(VNs),
                           syntax_errors(error),
                           module(embed)]),
	(Term = 'end_of_file'
	 -> (Terms = [], OptsOut = OptsIn)
	 ; (Terms = [(Term,VNs)|Rest],
	    debug(makeprog,'adding: ~w (variables: ~w)',[Term,VNs]),
            add_spec_clause(Term,VNs,Opts,OptsIn),
	    read_makeprog_stream(IO,OptsOut,Opts,Rest))).

eval_atom_as_makeprog_term(Atom,OptsOut,OptsIn) :-
        read_atom_as_makeprog_term(Atom,Term,VNs),
        debug(makeprog,'adding: ~w (variables: ~w)',[Term,VNs]),
        add_spec_clause(Term,VNs,OptsOut,OptsIn).

read_atom_as_makeprog_term(Atom,Term,VNs) :-
        read_term_from_atom(Atom,Term,[variable_names(VNs),
				       syntax_errors(error),
				       module(embed)]).

read_string_as_makeprog_term(String,Term,VNs) :-
        atom_string(Atom,String),
        read_atom_as_makeprog_term(Atom,Term,VNs).

translate_gnu_makefile(M,P) :-
    debug(makeprog,"Writing translated makefile to ~w",[P]),
    open(P,write,IO,[]),
    forall(member(G,M), write_clause(IO,G)),
    close(IO).

add_gnumake_clause(G,OptsOut,OptsIn) :-
    translate_gnumake_clause(G,P,VNs),
    !,
    add_spec_clause(P,VNs,OptsOut,OptsIn).

add_gnumake_clause(G,OptsOut,OptsIn) :-
    translate_gnumake_clause(G,P),
    add_spec_clause(P,OptsOut,OptsIn).

translate_gnumake_clause(rule(Ts,Ds,Es,{HeadGoal},{true},VNs), (Ts,{HeadGoal} <-- Ds,Es), VNs):- !.
translate_gnumake_clause(rule(Ts,Ds,Es,{HeadGoal},{DepGoal},VNs), (Ts,{HeadGoal} <-- Ds,{DepGoal},Es), VNs):- !.
translate_gnumake_clause(rule(Ts,Ds,Es,{DepGoal},VNs), (Ts <-- Ds,{DepGoal},Es), VNs):- !.
translate_gnumake_clause(prolog(Term,VNs), Term, VNs):- !.
translate_gnumake_clause(rule(Ts,Ds,Es), (Ts <-- Ds,Es)):- !.
translate_gnumake_clause(assignment(Var,"=",Val), (Var = Val)):- !.
translate_gnumake_clause(assignment(Var,"?=",Val), (Var ?= Val)):- !.
translate_gnumake_clause(assignment(Var,":=",Val), (Var := Val)):- !.
translate_gnumake_clause(assignment(Var,"+=",Val), (Var += Val)):- !.
translate_gnumake_clause(assignment(Var,"!=",Val), (Var =* Val)):- !.
translate_gnumake_clause(C,_) :-
    format("Error translating ~w~n",[C]),
	backtrace(20),
    fail.

write_clause(IO,option(Opt)) :-
    !,
    format(IO,"option(~w).~n",[Opt]).

write_clause(IO,rule(Ts,Ds,Es)) :-
    !,
    write_list(IO,Ts),
    write(IO,' <-- '),
    write_list(IO,Ds),
    (Es = []
     ; (write(IO,', '),
	write_list(IO,Es))),
    write(IO,'.\n').

write_clause(IO,rule(Ts,Ds,Es,{DepGoal},VNs)) :-
    !,
    write_list(IO,Ts),
    write(IO,' <-- '),
    write_list(IO,Ds),
    write(IO,', {'),
    write_term(IO,DepGoal,[variable_names(VNs),quoted(true)]),
    write(IO,'}'),
    (Es = []
     ; (write(IO,', '),
	write_list(IO,Es))),
    write(IO,'.\n').

write_clause(IO,rule(Ts,Ds,Es,{HeadGoal},{DepGoal},VNs)) :-
    !,
    write_list(IO,Ts),
    write(IO,', {'),
    write_term(IO,HeadGoal,[variable_names(VNs),quoted(true)]),
    write(IO,'}'),
    write(IO,' <-- '),
    write_list(IO,Ds),
    write(IO,', {'),
    write_term(IO,DepGoal,[variable_names(VNs),quoted(true)]),
    write(IO,'}'),
    (Es = []
     ; (write(IO,', '),
	write_list(IO,Es))),
    write(IO,'.\n').

write_clause(_,assignment(Var,_,_)) :-
    atom_codes(Var,[V|_]),
    V @>= 0'a, V @=< 0'z,   % a through z
    format("Prolog will not recognize `~w' as a variable, as it does not begin with an upper-case letter.~nStubbornly refusing to translate unless you fix this outrageous affront!~n",[Var]),
    halt_error.

write_clause(IO,assignment(Var,Op,Val)) :-
    format(IO,"~w ~w ~q.~n",[Var,Op,Val]).

write_clause(IO,prolog( (Term,VNs) )) :-
    !,
    write_term(IO,Term,[variable_names(VNs),quoted(true)]),
    write(IO,'.\n').

write_clause(_,X) :- format("Don't know how to write ~w~n",[X]).

write_list(IO,[X]) :- format(IO,"~q",[X]), !.
write_list(IO,L) :- format(IO,"~q",[L]).

add_cmdline_assignment((Var = X)) :-
        global_unbind(Var),
        assertz(global_cmdline_binding(Var,X)),
        debug(makeprog,'cmdline assign: ~w = ~w',[Var,X]).

add_spec_clause(Ass,Opts,Opts) :-
	Ass =.. [Op,Var,_],
	is_assignment_op(Op),
	!,
	add_spec_clause(Ass, [Var=Var], Opts, Opts).

add_spec_clause( Term, Opts, Opts ) :-
        add_spec_clause( Term, [], Opts, Opts ).

add_spec_clause( option(Opts), _VNs, OptsOut, OptsIn ) :-
	!,
	append(Opts,OptsIn,OptsOut).

add_spec_clause( (Var ?= X) , _VNs, Opts, Opts) :-
        global_binding(Var,Oldval),
        !,
        debug(makeprog,"Ignoring ~w = ~w since ~w is already bound to ~w",[Var,X,Var,Oldval]).


add_spec_clause( (Var ?= X), VNs, Opts, Opts) :-
        add_spec_clause((Var = X),VNs,Opts,Opts).

add_spec_clause( Ass, _VNs, Opts, Opts) :-
	Ass =.. [Op,Var,X],
	is_assignment_op(Op),
	\+ var(Var),
        global_cmdline_binding(Var,Oldval),
        !,
        debug(makeprog,"Ignoring ~w ~w ~w since ~w was bound to ~w on the command-line",[Var,Op,X,Var,Oldval]).

add_spec_clause( Ass, [], Opts, Opts) :-
	Ass =.. [Op,Var,_],
	is_assignment_op(Op),
	atom_codes(Var,[V|_]),
	V @>= 0'a, V @=< 0'z,   % a through z
        debug(makeprog,"Warning: Prolog will not recognize ~w as a variable as it does not begin with an upper-case letter. Use at your own peril!~n",[Var]),
	fail.

add_spec_clause( (Var = X), VNs, Opts, Opts) :-
	!,
        member(Var=Var,VNs),
        global_unbind(Var),
        assertz(global_lazy_binding(Var,X)),
        debug(makeprog,'assign: ~w = ~w',[Var,X]).

add_spec_clause( (Var := X,{Goal}), VNs, Opts, Opts) :-
        !,
        member(Var=Var,VNs),
        normalize_pattern(X,Y,v(_,_,_,VNs)),
        findall(Y,Goal,Ys),
	unwrap_t(Ys,Yflat),  % hack; parser adds unwanted t(...) wrapper
	!,
        global_unbind(Var),
        assertz(global_simple_binding(Var,Yflat)),
        debug(makeprog,'assign: ~w := ~w',[Var,Yflat]).

add_spec_clause( (Var := X), VNs, Opts, Opts) :-
        !,
        add_spec_clause( (Var := X,{true}), VNs, Opts, Opts).

add_spec_clause( (Var += X), VNs, Opts, Opts) :-
        !,
        member(Var=Var,VNs),
        normalize_pattern(X,Y,v(_,_,_,VNs)),
	unwrap_t(Y,Yflat),  % hack; parser adds too many t(...)'s
	!,
	(global_binding(Var,Old),
	 concat_string_list([Old," ",Yflat],New);
	 New = Yflat),
        global_unbind(Var),
        assertz(global_simple_binding(Var,New)),
        debug(makeprog,'assign: ~w := ~w',[Var,New]).

add_spec_clause( (Var =* X), VNs, Opts, Opts) :-
        !,
        member(Var=Var,VNs),
	shell_eval_str(X,Y),
	!,
        global_unbind(Var),
        assertz(global_lazy_binding(Var,Y)),
        debug(makeprog,'assign: ~w =* ~w  ==>  ~w',[Var,X,Y]).

add_spec_clause( (Head,{HeadGoal} <-- Deps,{DepGoal},Exec), VNs, Opts, Opts) :-
        !,
        add_spec_clause(mkrule(Head,Deps,Exec,HeadGoal,DepGoal),VNs,Opts,Opts).
add_spec_clause( (Head,{HeadGoal} <-- Deps,{DepGoal}), VNs, Opts, Opts) :-
        !,
        add_spec_clause(mkrule(Head,Deps,[],HeadGoal,DepGoal),VNs,Opts,Opts).
add_spec_clause( (Head,{HeadGoal} <-- Deps, Exec), VNs, Opts, Opts) :-
        !,
        add_spec_clause(mkrule(Head,Deps,Exec,HeadGoal,true),VNs,Opts,Opts).
add_spec_clause( (Head,{HeadGoal} <-- Deps), VNs, Opts, Opts) :-
        !,
        add_spec_clause(mkrule(Head,Deps,[],HeadGoal,true),VNs,Opts,Opts).

add_spec_clause( (Head <-- Deps,{DepGoal},Exec), VNs, Opts, Opts) :-
        !,
        add_spec_clause(mkrule(Head,Deps,Exec,DepGoal),VNs,Opts,Opts).
add_spec_clause( (Head <-- Deps,{DepGoal}), VNs, Opts, Opts) :-
        !,
        add_spec_clause(mkrule(Head,Deps,[],DepGoal),VNs,Opts,Opts).
add_spec_clause( (Head <-- Deps, Exec), VNs, Opts, Opts) :-
        !,
        add_spec_clause(mkrule(Head,Deps,Exec),VNs,Opts,Opts).
add_spec_clause( (Head <-- Deps), VNs, Opts, Opts) :-
        !,
        add_spec_clause(mkrule(Head,Deps,[]),VNs,Opts,Opts).

add_spec_clause(Rule,VNs,Opts,Opts) :-
        Rule =.. [mkrule,T|_],
        !,
        debug(makeprog,'with: ~w ~w',[Rule,VNs]),
	set_default_target(T),
        assertz(with(Rule,VNs)).

add_spec_clause(Term,_,Opts,Opts) :-
        debug(makeprog,"assert ~w",Term),
	expand_term(Term,Expanded),
        assertz(Expanded).

set_default_target(_) :-
	default_target(_),
	debug(makeprog,"Default target already set",[]),
	!.
set_default_target([T|_]) :-
	expand_vars_head(T,Tx),
	\+ string_chars(T,['.'|_]),
	equal_as_strings(T,Tx),  % only set default target if T contains no variables
	!,
	debug(makeprog,"Setting default target to ~s",[Tx]),
	assertz(default_target(Tx)).
set_default_target([_|_]) :- !.
set_default_target(T) :- set_default_target([T]).

global_unbind(Var) :-
	retractall(global_cmdline_binding(Var,_)),
	retractall(global_simple_binding(Var,_)),
	retractall(global_lazy_binding(Var,_)).

global_binding(Var,Val) :- global_cmdline_binding(Var,Val).
global_binding(Var,Val) :- global_simple_binding(Var,Val).
global_binding(Var,Val) :- global_lazy_binding(Var,Val).

% ----------------------------------------
% RULES AND PATTERN MATCHING
% ----------------------------------------

target_bindrule_exact(T) :-
        mkrule_default(TP1,_,_,HeadGoal,_,Bindings),
	V=v(null,T,_,Bindings),
        normalize_patterns(TP1,TPs,V),
	member(TP,TPs),
	exact_match(TP,T),
	setauto('TARGET',T,Bindings),
	call_without_backtrace(HeadGoal).

target_bindrule(T,rb(T,Ds,DepGoal,Exec1,V),_Opts) :-
        mkrule_default(TP1,DP1,Exec1,HeadGoal,DepGoal,Bindings),
	debug(bindrule,"rule: T=~w TP1=~w DP1=~w E1=~w HG=~w DG=~w B=~w",[T,TP1,DP1,Exec1,HeadGoal,DepGoal,Bindings]),
        append(Bindings,_,Bindings_Open),
        V=v(_Base,T,Ds,Bindings_Open),
        normalize_patterns(TP1,TPs,V),

        % we allow multiple heads;
        % only one of the specified targets has to match
        member(TP,TPs),
        pattern_match(TP,T),

	% Check the HeadGoal
	setauto('TARGET',T,Bindings),
	call_without_backtrace(HeadGoal),

	% Do a two-pass expansion of dependency list.
	% This is ultra-hacky but allows for variable-expanded dependency lists that contain % wildcards
	% (the variables are expanded on the first pass, and the %'s on the second pass).
	% A more rigorous solution would be a two-pass expansion of the entire GNU Makefile,
	% which would allow currently impossible things like variable-expanded rules, e.g.
	%   RULE = target: dep1 dep2
	%   $(RULE) dep3
	% which (in GNU make, but not here) expands to
	%   target: dep1 dep2 dep3
	% However, this would fragment the current homology between the Prolog syntax and GNU Make syntax,
	% making it harder to translate GNU Makefiles into Prolog.
	% Consequently, we currently sacrifice perfect GNU make compatibility for a simpler translation.
	expand_deps(DP1,DP2,V),
	expand_deps(DP2,Ds,V),

	% Set up the DepGoal
	setauto('DEPS',Ds,Bindings),

	% and, success
	debug(bindrule,"rule matched",[]).

dep_bindrule(rb(T,Ds,true,Exec1,V),Opts,rb(T,Ds,true,Execs,V),[qsub_use_biomake(true)|Opts]) :-
	get_opt(md5,true,Opts),
	!,
	expand_execs(Exec1,Execs,V).

dep_bindrule(rb(T,Ds,true,Exec1,V),Opts,rb(T,Ds,true,Execs,V),Opts) :-
	!,
	expand_execs(Exec1,Execs,V).

dep_bindrule(rb(T,Ds,DepGoal,Exec1,V),Opts,rb(T,Ds,true,Execs,V),[qsub_use_biomake(true)|Opts]) :-
	(call_without_backtrace(DepGoal)
         ; building_asynchronously(Opts)),
	expand_execs(Exec1,Execs,V).

setauto(VarLabel,Value,Bindings) :-
	member((VarLabel = Value), Bindings),
	!.
setauto(_,_,_).

exact_match(t(TL),A) :- !, exact_match(TL,A).
exact_match([],'').
exact_match([Tok|PatternToks],Atom) :-
    nonvar(Tok),
    !,
    atom_concat(Tok,Rest,Atom),
    exact_match(PatternToks,Rest).
exact_match([Tok|PatternToks],Atom) :-
    var(Tok),
    !,
    atom_concat(Tok,Rest,Atom),
    Tok\='',
    exact_match(PatternToks,Rest).

pattern_match(A,B) :- var(A),!,B=A.
pattern_match(t(TL),A) :- !, pattern_match(TL,A).
pattern_match([],'').
pattern_match([Tok|PatternToks],Atom) :-
    nonvar(Tok),
    !,
    atom_concat(Tok,Rest,Atom),
    pattern_match(PatternToks,Rest).
pattern_match([Tok|PatternToks],Atom) :-
    var(Tok),
    !,
    atom_concat(Tok,Rest,Atom),
    Tok\='',
    pattern_match(PatternToks,Rest).

pattern_match_list([],[]).
pattern_match_list([P|Ps],[M|Ms]) :-
        pattern_match(P,M),
        pattern_match_list(Ps,Ms).

expand_deps(Deps,Result,V) :-
    normalize_patterns(Deps,NormDeps,V),
    maplist(unwrap_t,NormDeps,ExpandedDeps),
    maplist(split_spaces,ExpandedDeps,DepLists),
    flatten_trim(DepLists,Result).

expand_execs(Execs,Result,V) :-
    normalize_patterns_body(Execs,NormExecs,V),
    maplist(unwrap_t,NormExecs,ExpandedExecs),
    maplist(split_newlines,ExpandedExecs,ExecLists),
    flatten_trim(ExecLists,Result).

flatten_trim(Lumpy,Trimmed) :-
    flatten(Lumpy,Untrimmed),
    include(not_empty,Untrimmed,Trimmed).

not_empty(X) :- X \= "", X \= ''.

% ----------------------------------------
% PATTERN SYNTAX AND API
% ----------------------------------------

:- multifile
        mkrule/3,
        mkrule/4,
        mkrule/5,
        with/2.
:- dynamic
        mkrule/3,
        mkrule/4,
        mkrule/5,
        with/2.

mkrule_default(T,D,E,Ghead,Gdep,VNs) :- with(mkrule(T,D,E,Ghead,Gdep),VNs).
mkrule_default(T,D,E,true,Gdep,VNs) :- with(mkrule(T,D,E,Gdep),VNs).
mkrule_default(T,D,E,true,true,VNs) :- with(mkrule(T,D,E),VNs).

expand_vars_head(X,Y) :-
	expand_vars_head(X,Y,v(null,null,null,[])).

expand_vars_head(X,Y,V) :-
	normalize_pattern(X,Yt,V),
	unwrap_t(Yt,Y).

normalize_patterns(X,X,_) :- var(X),!.
normalize_patterns([],[],_) :- !.
normalize_patterns([P|Ps],[N|Ns],V) :-
        !,
        debug(pattern,'*norm: ~w',[P]),
        normalize_pattern(P,N,V),
        normalize_patterns(Ps,Ns,V).
normalize_patterns(P,Ns,V) :-
        normalize_pattern(P,N,V),
	wrap_t(N,Ns).

% this is a bit hacky - parsing is too eager to add t(...) wrapper (original comment by cmungall)
% Comment by ihh: not entirely sure what all this wrapping evaluated patterns in t(...) is about.
% It seems to be some kind of a marker for pattern evaluation.
% Anyway...
% wrap_t is a construct from cmungall's original code, abstracted into a separate term by me (ihh).
% unwrap_t flattens a list into an atom, removing any t(...) wrappers in the process,
% and evaluating any postponed functions wrapped with a call(...) compound clause.
wrap_t(t([L]),L) :- member(t(_),L), !.
wrap_t(X,[X]).

%unwrap_t(_,_) :- backtrace(20), fail.
unwrap_t(X,'') :- var(X), !.
unwrap_t(Call,Flat) :- nonvar(Call), Call =.. [call,_|_], !, unwrap_t_call(Call,F), unwrap_t(F,Flat).
unwrap_t(t(X),Flat) :- unwrap_t(X,Flat), !.
unwrap_t([],'') :- !.
unwrap_t([L|Ls],Flat) :- unwrap_t(L,F), unwrap_t(Ls,Fs), atom_concat(F,Fs,Flat), !.
unwrap_t(N,A) :- number(A), atom_number(A,N), !.
unwrap_t(S,A) :- string(S), atom_string(A,S), !.
unwrap_t(S,S) :- ground(S), !.
unwrap_t(X,_) :- type_of(X,T), format("Can't unwrap ~w ~w~n",[T,X]), fail.

unwrap_t_call(call(X,Y),Result) :- !, unwrap_t_call(Y,Yret), call(X,Yret,Result).
unwrap_t_call(call(X,Y,Z),Result) :- !, unwrap_t_call(Z,Zret), call(X,Y,Zret,Result).
unwrap_t_call(R,R).

normalize_pattern(X,X,_) :- var(X),!.
normalize_pattern(t(X),t(X),_) :- !.
normalize_pattern(Term,t(Args),_) :-
        Term =.. [t|Args],!.
normalize_pattern(X,t(Toks),V) :-
        debug(pattern,'PARSING: ~w // ~w',[X,V]),
        atom_chars(X,Chars),
        phrase(head_toks(Toks,V),Chars),
        debug(pattern,'PARSED: ~w ==> ~w',[X,Toks]),
%	backtrace(20),
        !.

expand_vars(X,Y) :-
	expand_vars(X,Y,v(null,null,null,[])).

expand_vars(X,Y,V) :-
	normalize_pattern_body(X,Yt,V),
	unwrap_t(Yt,Y).

normalize_patterns_body(X,X,_) :- var(X),!.
normalize_patterns_body([],[],_) :- !.
normalize_patterns_body([P|Ps],[N|Ns],V) :-
        !,
        debug(pattern,'*norm: ~w',[P]),
        normalize_pattern_body(P,N,V),
        normalize_patterns_body(Ps,Ns,V).
normalize_patterns_body(P,Ns,V) :-
        normalize_pattern_body(P,N,V),
	wrap_t(N,Ns).

normalize_pattern_body(X,X,_) :- var(X),!.
normalize_pattern_body(t(X),t(X),_) :- !.
normalize_pattern_body(Term,t(Args),_) :-
        Term =.. [t|Args],!.
normalize_pattern_body(X,t(Toks),V) :-
        debug(pattern,'PARSING: ~w // ~w',[X,V]),
        atom_chars(X,Chars),
        phrase(body_toks(Toks,V),Chars),
        debug(pattern,'PARSED: ~w ==> ~w',[X,Toks]),
        !.

body_toks([],_) --> [].
body_toks([Tok|Toks],V) --> body_tok(Tok,V),!,body_toks(Toks,V).
body_tok('%',_) --> ['%'], !.
body_tok(Tok,V) --> tok(Tok,V).

head_toks([],_) --> [].
head_toks([Tok|Toks],V) --> head_tok(Tok,V),!,head_toks(Toks,V).
head_tok(Var,V) --> ['%'],!,{bindvar_debug('%',V,Var)}.
head_tok(Tok,V) --> tok(Tok,V).

tok('$',_V) --> ['$','$'], !.  % escape $'s
tok(Var,V) --> ['$'], varlabel(VL),{bindvar_debug(VL,V,Var)}.
tok(Var,V) --> ['$'], makefile_subst_ref(Var,V), !.
tok(Var,V) --> ['$'], makefile_computed_var(Var,V), !.
tok(Var,V) --> ['$'], makefile_function(Var,V), !.
tok("$",_V) --> ['$'], !.   % if all else fails, let the dollar through
tok(Tok,_) --> tok_a(Cs),{atom_chars(Tok,Cs)}.
tok_a([C|Cs]) --> [C],{C\='$',C\='%'},!,tok_a(Cs).
tok_a([]) --> [].
varlabel('<') --> ['<'],!.
varlabel('*') --> ['*'],!.
varlabel('@') --> ['@'],!.
varlabel('^') --> ['^'],!.
varlabel('+') --> ['^'],!.  % $+ is not quite the same as $^, but we fudge it
varlabel('?') --> ['^'],!.  % $? is not quite the same as $^, but we fudge it
varlabel('<') --> bracketed(['<']),!.
varlabel('*') --> bracketed(['*']),!.
varlabel('@') --> bracketed(['@']),!.
varlabel('^') --> bracketed(['^']),!.
varlabel('^') --> bracketed(['+']),!.
varlabel('^') --> bracketed(['?']),!.
varlabel('*F') --> bracketed(['*','F']),!.
varlabel('*D') --> bracketed(['*','D']),!.
varlabel('@F') --> bracketed(['@','F']),!.
varlabel('@D') --> bracketed(['@','D']),!.
varlabel('<F') --> bracketed(['<','F']),!.
varlabel('<D') --> bracketed(['<','D']),!.
varlabel('^F') --> bracketed(['^','F']),!.
varlabel('^D') --> bracketed(['^','D']),!.
varlabel('^F') --> bracketed(['+','F']),!.
varlabel('^D') --> bracketed(['+','D']),!.
varlabel('^F') --> bracketed(['?','F']),!.
varlabel('^D') --> bracketed(['?','D']),!.
varlabel(A) --> makefile_var_char(C), {atom_chars(A,[C])}.
varlabel(A) --> ['('],makefile_var_atom_from_chars(A),[')'].
varlabel(A) --> ['{'],makefile_var_atom_from_chars(A),['}'].

bracketed(L) --> ['('],L,[')'].
bracketed(L) --> ['{'],L,['}'].

bindvar(VL,v(S,T,D,BL),X) :- bindauto(VL,v(S,T,D,BL),X), !.
bindvar(VL,v(_,_,_,_),X) :- global_cmdline_binding(VL,X),!.
bindvar(VL,v(_,_,_,_),X) :- global_simple_binding(VL,X),!.
bindvar(VL,v(_,_,_,_),X) :- getenv(VL,X).
bindvar(VL,v(V1,V2,V3,BL),X) :-
	global_lazy_binding(VL,Y),
	append(BL,[VL=VL],BL2),
	normalize_pattern(Y,Z,v(V1,V2,V3,BL2)),
	unwrap_t(Z,X),
	!.
bindvar(VL,v(_,_,_,BL),X) :- member(VL=X,BL),!.
bindvar(_,v(_,_,_,_),'') :- !.  % default: bind to empty string

bindauto('%',v(X,_,_,_),X) :- !.
bindauto('*',v(X,_,_,_),X) :- !.
bindauto('@',v(_,X,_,_),X) :- !.
bindauto('<',v(_,_,[X|_],_),X) :- !.
bindauto('^',v(_,_,X,_),call(concat_string_list_spaced,X)) :- !.
bindauto('*F',v(X,_,_,_),call(file_base_name,X)) :- !.
bindauto('*D',v(X,_,_,_),call(file_directory_name,X)) :- !.
bindauto('@F',v(_,X,_,_),call(file_base_name,X)) :- !.
bindauto('@D',v(_,X,_,_),call(file_directory_name,X)) :- !.
bindauto('<F',v(_,_,[X|_],_),call(file_base_name,X)) :- !.
bindauto('<D',v(_,_,[X|_],_),call(file_directory_name,X)) :- !.
bindauto('^F',v(_,_,X,_),call(concat_string_list_spaced,call(maplist,file_base_name,X))) :- !.
bindauto('^D',v(_,_,X,_),call(concat_string_list_spaced,call(maplist,file_directory_name,X))) :- !.

% debugging variable binding
bindvar_debug(VL,V,Var) :-
    debug(pattern,"binding ~w",[VL]),
    %show_global_bindings,
    bindvar(VL,V,Var),
    debug(pattern,"bound ~w= ~w",[VL,Var]).

show_global_bindings :-
    forall(global_binding(Var,Val),
	   format("global binding: ~w = ~w\n",[Var,Val])).
    
