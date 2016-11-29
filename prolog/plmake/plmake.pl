% * -*- Mode: Prolog -*- */

:- module(plmake,
          [
           build_default/0,
           build_default/1,

           build/1,
           build/2,
	   
           consult_makeprog/1,
           consult_gnu_makefile/1,

	   add_spec_clause/1,
	   add_spec_clause/2,
	   add_assignment/1,
	   
           target_bindrule/2,
           rule_dependencies/3,
           is_rebuild_required/4,

	   expand_vars/2
           ]).

:- use_module(library(plmake/functions)).

/** <module> Prolog implementation of Makefile-inspired build system

  See the README

  */



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

build_default :-
	build_default([]).

build_default(Opts) :-
	default_target(T),
	build(T,Opts).

build :-
	format("No targets. Stop.~n").

build(T) :-
        build(T,[]).
build(T,Opts) :-
        build(T,[],Opts).

build(T,SL,Opts) :-
        %show_global_bindings,
        %report('Target: ~w',[T]),
        debug_report(build,'  Target: ~w',[T],SL),
        target_bindrule(T,Rule),
        debug_report(build,'  Bindrule: ~w',[Rule],SL),
        rule_dependencies(Rule,DL,Opts),
        report('Checking dependencies: ~w <-- ~w',[T,DL],SL,Opts),
        !,
        build_targets(DL,[T|SL],Opts), % semidet
        (   is_rebuild_required(T,DL,SL,Opts)
        ->  rule_execs(Rule,Execs,Opts),
            run_execs(Execs,Opts)
        ;   true),
        report('~w is up to date',[T],SL,Opts).
build(T,SL,Opts) :-
        debug_report(build,'  checking if rebuild required for ~w',[T],SL),
        \+ is_rebuild_required(T,[],SL,Opts),
        !,
        report('~w exists',[T],SL,Opts).
build(T,SL,Opts) :-
        report('~w FAILED',[T],SL,Opts),
        !,
        fail.

% potentially split this into two steps:
%  phase 1 iterates through targets and initiates any jobs
%  phase 2 checks all jobs are done
build_targets([],_,_).
build_targets([T|TL],SL,Opts) :-
        build(T,SL,Opts),
        build_targets(TL,SL,Opts).

% ----------------------------------------
% REPORTING
% ----------------------------------------

report(Fmt,Args,Opts) :-
        report(Fmt,Args,[],Opts).

report(Fmt,Args,SL,_) :-
        stack_indent(SL,Fmt,IndentedFmt),
        format(IndentedFmt,Args),
        nl.

stack_indent([],Text,Text).
stack_indent([_|T],Text,Indented) :-
        string_concat('    ',Text,Tab),
	stack_indent(T,Tab,Indented).

debug_report(Topic,Fmt,Args) :-
        debug_report(Topic,Fmt,Args,[]).

debug_report(Topic,Fmt,Args,SL) :-
        stack_indent(SL,Fmt,IndentedFmt),
	debug(Topic,IndentedFmt,Args).

% ----------------------------------------
% DEPENDENCY MANAGEMENT
% ----------------------------------------

is_rebuild_required(T,_,SL,Opts) :-
        \+ exists_target(T,Opts),
        !,
        report('Target ~w not materialized - will rebuild if required',[T],SL,Opts).
is_rebuild_required(T,DL,SL,Opts) :-
        \+ exists_directory(T),
        member(D,DL),
        is_built_after(D,T,Opts),
        !,
        report('Target ~w built before dependency ~w - rebuilding',[T,D],SL,Opts).
is_rebuild_required(T,_,SL,Opts) :-
        member(always_make(true),Opts),
        target_bindrule(T,_),
        !,
        report('Specified --always-make; rebuilding target ~w',[T],SL,Opts).

is_built_after(A,B,_Opts) :-
        time_file(A,TA),
        time_file(B,TB),
        TA > TB.

exists_target(T,_Opts) :-
        exists_file(T).
exists_target(T,_Opts) :-
        exists_directory(T).


rule_dependencies(rb(_,DL,_),DL,_Opts).
rule_execs(rb(_,_,X),X,_Opts) :- !.
rule_execs(rb(_,_,X),_,_Opts) :- throw(error(no_exec(X))).


% ----------------------------------------
% TASK EXECUTION
% ----------------------------------------

run_execs([],_).
run_execs([E|Es],Opts) :-
        run_exec(E,Opts),
        run_execs(Es,Opts).

run_exec(Exec,Opts) :-
        member(dry_run(true),Opts),
        !,
        report('~w',[Exec],Opts).
run_exec(Exec,Opts) :-
        report('~w',[Exec],Opts),
        get_time(T1),
        shell(Exec,Err),
        get_time(T2),
        DT is T2-T1,
        debug_report(build,'  Return: ~w Time: ~w',[Err,DT],Opts),
        Err=0,
        !.
run_exec(Exec,_Opts) :-
        throw(error(run(Exec))).

% ----------------------------------------
% RULES AND PATTERN MATCHING
% ----------------------------------------



target_bindrule(T,rb(T,Ds,Execs)) :-
        mkrule_normalized(TPs,DPs,ExecPs,Goal),
        % we allow multiple heads;
        % only one of the specified targets has to match
        member(TP,TPs),
        debug(bindrule,'  pre TP/DPs: ~w / ~w ==> ~w',[TP,DPs,ExecPs]),
        uniq_pattern_match(TP,T),
        debug(bindrule,'  pst1 TP/DPs: ~w / ~w ==> ~w :: ~w',[TP,DPs,ExecPs,Goal]),
        Goal,
        debug(bindrule,'  pst TP/DPs: ~w / ~w ==> ~w',[TP,DPs,ExecPs]),
        maplist(pattern_target,DPs,SpacedDeps),
        maplist(split_spaces,SpacedDeps,DepLists),
	flatten(DepLists,Ds),
        maplist(pattern_eval,ExecPs,Execs).

split_spaces(S,L) :-
	split_string(S," "," ",L).


% semidet
uniq_pattern_match(TL,A) :-
        debug(bindrule,'Matching: ~w to ~w',[TL,A]),
        pattern_match(TL,A),
        debug(bindrule,' Matched: ~w to ~w',[TL,A]),
        !.
uniq_pattern_match(TL,A) :-
        debug(bindrule,' NO_MATCH: ~w to ~w',[TL,A]),
        fail.


pattern_target(t(TL),A) :- atomic_list_concat(TL,A).
pattern_eval(t(TL),A) :-
    flatten(TL,TL2),
    include(var,TL2,TL2_unbound),
    (TL2_unbound = [] -> true; debug(pattern,"Warning: unbound variables in ~w~n",[TL2])),
    include(nonvar,TL2,TL2_bound),
    atomic_list_concat(TL2_bound,A).
%pattern_eval(c(TL),A) :- atomic_list_concat(TL,A).


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

% ----------------------------------------
% READING
% ----------------------------------------

:- dynamic global_cmdline_binding/2.
:- dynamic global_simple_binding/2.
:- dynamic global_lazy_binding/2.

:- dynamic default_target/1.

:- user:op(1100,xfy,<--).

:- user:op(1101,xfy,?=).
:- user:op(1102,xfy,:=).
:- user:op(1103,xfy,+=).

consult_gnu_makefile(F) :-
        ensure_loaded(library(plmake/gnumake_parser)),
        parse_gnu_makefile(F,M),
        forall(member(L,M),
	       ((L = rule(Ts,Ds,Es)) -> add_spec_clause((Ts <-- Ds,Es),[]);
		((L = assignment(Var,Op,Val)) ->
		     (Op = "=" -> add_spec_clause((Var = Val));
		      (Op = "?=" -> add_spec_clause((Var ?= Val));
		       (Op = ":=" -> add_spec_clause((Var := Val));
			(Op = "+=" -> add_spec_clause((Var += Val));
			    true))));
		 true))).

consult_makeprog(F) :-
        debug(makeprog,'reading: ~w',[F]),
        open(F,read,IO,[]),
        repeat,
        (   at_end_of_stream(IO)
        ->  !
        ;   read_term(IO,Term,[variable_names(VNs),
                               syntax_errors(error),
                               module(plmake)]),
            debug(makeprog,'adding: ~w',[Term]),
            add_spec_clause(Term,VNs),
            fail),
        debug(makeprog,'read: ~w',[F]),
        close(IO).


add_assignment((Var = X)) :-
        global_unbind(Var),
        assert(global_cmdline_binding(Var,X)),
        debug(makeprog,'cmdline assign: ~w = ~w',[Var,X]).


add_spec_clause((Var = X)) :-
	add_spec_clause((Var = X), [Var=Var]).
add_spec_clause((Var ?= X)) :-
	add_spec_clause((Var ?= X), [Var=Var]).
add_spec_clause((Var := X)) :-
	add_spec_clause((Var := X), [Var=Var]).
add_spec_clause((Var += X)) :-
	add_spec_clause((Var += X), [Var=Var]).


add_spec_clause( (Var ?= X) ,_VNs) :-
        global_binding(Var,Oldval),
        !,
        debug(makeprog,"Ignoring ~w = ~w since ~w is already bound to ~w",[Var,X,Var,Oldval]).

add_spec_clause( (Var ?= X) ,VNs) :-
        add_spec_clause((Var = X),VNs).

add_spec_clause( (Var = X) ,_VNs) :-
        global_cmdline_binding(Var,Oldval),
        !,
        debug(makeprog,"Ignoring ~w = ~w since ~w was bound to ~w on the command-line",[Var,X,Var,Oldval]).

add_spec_clause( (Var = X) ,VNs) :-
	!,
        member(Var=Var,VNs),
        global_unbind(Var),
        assert(global_lazy_binding(Var,X)),
        debug(makeprog,'assign: ~w = ~w',[Var,X]).

add_spec_clause( (Var := X) ,_VNs) :-
        global_cmdline_binding(Var,Oldval),
        !,
        debug(makeprog,"Ignoring ~w := ~w since ~w was bound to ~w on the command-line",[Var,X,Var,Oldval]).

add_spec_clause( (Var := X,{Goal}) ,VNs) :-
        !,
        member(Var=Var,VNs),
        normalize_pattern(X,Y,v(_,_,_,VNs)),
        findall(Y,Goal,Ys),
	unwrap_t(Ys,Yflat),  % hack; parser adds unwanted t(...) wrapper
	!,
        global_unbind(Var),
        assert(global_simple_binding(Var,Yflat)),
        debug(makeprog,'assign: ~w := ~w',[Var,Yflat]).

add_spec_clause( (Var := X) ,VNs) :-
        !,
        add_spec_clause( (Var := X,{true}) ,VNs).

add_spec_clause( (Var += X) ,_VNs) :-
        global_cmdline_binding(Var,Oldval),
        !,
        debug(makeprog,"Ignoring ~w += ~w since ~w was bound to ~w on the command-line",[Var,X,Var,Oldval]).

add_spec_clause( (Var += X) ,VNs) :-
        !,
        member(Var=Var,VNs),
        normalize_pattern(X,Y,v(_,_,_,VNs)),
	unwrap_t(Y,Yflat),  % hack; parser adds too many t(...)'s
	!,
	(global_binding(Var,Old),
	 concat_string_list([Old," ",Yflat],New);
	 New = Yflat),
        global_unbind(Var),
        assert(global_simple_binding(Var,New)),
        debug(makeprog,'assign: ~w := ~w',[Var,New]).

add_spec_clause( (Head <-- Deps,{Goal},Exec) ,VNs) :-
        !,
        add_spec_clause(mkrule(Head,Deps,Exec,Goal),VNs).
add_spec_clause( (Head <-- Deps,{Goal}) ,VNs) :-
        !,
        add_spec_clause(mkrule(Head,Deps,[],Goal),VNs).
add_spec_clause( (Head <-- Deps, Exec) ,VNs) :-
        !,
        add_spec_clause(mkrule(Head,Deps,Exec),VNs).
add_spec_clause( (Head <-- Deps) ,VNs) :-
        !,
        add_spec_clause(mkrule(Head,Deps,[]),VNs).

add_spec_clause(Rule,VNs) :-
        Rule =.. [mkrule,T|_],
        !,
        debug(makeprog,'with: ~w ~w',[Rule,VNs]),
	set_default_target(T),
        assert(with(Rule,VNs)).

add_spec_clause(Term,_) :-
        assert(Term).

set_default_target(_) :-
	default_target(_),
	!.
set_default_target([T|_]) :-
	!,
	expand_vars(T,Tx),
	debug(makeprog,"Setting default target to ~s",[Tx]),
	assert(default_target(Tx)).
set_default_target(_).

global_unbind(Var) :-
	retractall(global_cmdline_binding(Var,_)),
	retractall(global_simple_binding(Var,_)),
	retractall(global_lazy_binding(Var,_)).

global_binding(Var,Val) :- global_cmdline_binding(Var,Val).
global_binding(Var,Val) :- global_simple_binding(Var,Val).
global_binding(Var,Val) :- global_lazy_binding(Var,Val).

% ----------------------------------------
% PATTERN SYNTAX AND API
% ----------------------------------------

:- multifile
        mkrule/3,
        mkrule/4,
        with/2.
:- dynamic
        mkrule/3,
        mkrule/4,
        with/2.

mkrule_default(T,D,E,true,VNs) :- with(mkrule(T,D,E),VNs).
mkrule_default(T,D,E,G,VNs) :- with(mkrule(T,D,E,G),VNs).

mkrule_normalized(TPs,DPs,ExecPs,Goal) :-
        mkrule_default(TP1,DP1,Exec1,Goal,Bindings),
        append(Bindings,_,Bindings_Open),
        V=v(_Base,InitT,InitD,Bindings_Open),
        normalize_patterns(TP1,TPs,V),
        normalize_patterns(DP1,DPs,V),
        (   TPs=[t(InitT)|_]
        ->  true
        ;   true),
        (   DPs=[t(InitD)|_]
        ->  true
        ;   true),
        normalize_patterns(Exec1,ExecPs,V).

expand_vars(X,Y) :-
	normalize_pattern(X,Yt,v("","","",[])),
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
	wrap_t(N,Ns). % this is a bit hacky - parsing is too eager to add t(...) wrapper

wrap_t(t([L]),L) :- member(t(_),L).
wrap_t(L,[L]).

unwrap_t([t([Flat])],Flat).
unwrap_t([t(L)],Flat) :- concat_string_list(L,Flat).
unwrap_t(X,Flat) :- unwrap_t([X],Flat).
unwrap_t(L,L).

concat_string_list([],"").
concat_string_list([L|Ls],F) :- concat_string_list(Ls,R), string_concat(L,R,F).

normalize_pattern(X,X,_) :- var(X),!.
normalize_pattern(t(X),t(X),_) :- !.
normalize_pattern(Term,t(Args),_) :-
        Term =.. [t|Args],!.
normalize_pattern(X,t(Toks),V) :-
        debug(pattern,'PARSING: ~w // ~w',[X,V]),
        atom_chars(X,Chars),
        phrase(toks(Toks,V),Chars),
        debug(pattern,'PARSED: ~w ==> ~w',[X,Toks]),
        !.

toks([],_) --> [].
toks([Tok|Toks],V) --> tok(Tok,V),!,toks(Toks,V).
tok(Var,_V) --> makefile_function(Var), !.
tok(Var,V) --> ['%'],!,{bindvar_debug('%',V,Var)}.
tok(Var,V) --> ['$'],varlabel(VL),!,{bindvar_debug(VL,V,Var)}.
tok(Tok,_) --> tok_a(Cs),{atom_chars(Tok,Cs)}.
tok_a([C|Cs]) --> [C],{C\='$',C\='%'},!,tok_a(Cs).
tok_a([]) --> [].
varlabel('<') --> ['<'],!.
varlabel('*') --> ['*'],!.
varlabel('@') --> ['@'],!.
varlabel(A) --> alphanum(C),{atom_chars(A,[C])}.
varlabel(A) --> ['('],alphanum(C),alphanums(Cs),[')'],{atom_chars(A,[C|Cs])}.
alphanums([X|Xs]) --> alphanum(X),!,alphanums(Xs).
alphanums([]) --> [].
alphanum(X) --> [X],{X@>='a',X@=<'z'},!.
alphanum(X) --> [X],{X@>='A',X@=<'Z'},!.
alphanum(X) --> [X],{X@>='0',X@=<'9'},!.    % foo('0') %
alphanum('_') --> ['_'].

bindvar('%',v(X,_,_,_),X) :- !.
bindvar('*',v(X,_,_,_),X) :- !.
bindvar('@',v(_,X,_,_),X) :- !.
bindvar('<',v(_,_,X,_),X) :- !.
bindvar(VL,v(_,_,_,_),X) :- global_cmdline_binding(VL,X),!.
bindvar(VL,v(_,_,_,_),X) :- global_simple_binding(VL,X),!.
bindvar(VL,v(_,_,_,_),X) :- global_lazy_binding(VL,Y),normalize_pattern(Y,Z,v(_,_,_,[VL=VL])),unwrap_t(Z,X),!.
bindvar(VL,v(_,_,_,BL),X) :- member(VL=X,BL),!.
bindvar(_,_,'') :- !.

% debugging variable binding
bindvar_debug(VL,V,Var) :-
    debug(pattern,"binding ~w",[VL]),
    %show_global_bindings,
    bindvar(VL,V,Var),
    debug(pattern,"bound ~w= ~w",[VL,Var]).

show_global_bindings :-
    forall(global_binding(Var,Val),
	   format("global binding: ~w = ~w\n",[Var,Val])).
    
