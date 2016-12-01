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
	   add_cmdline_assignment/1,
	   add_gnumake_clause/1,
	   
           target_bindrule/2,
           rule_dependencies/3,
           is_rebuild_required/4,

	   global_binding/2,
	   expand_vars/3
           ]).

:- use_module(library(plmake/utils)).
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
            run_execs(Execs,Opts),
	    flag_as_rebuilt(T)
        ;   true),
        (member(dry_run(true),Opts) -> true;
         report('~w is up to date',[T],SL,Opts)).
build(T,SL,Opts) :-
        debug_report(build,'  checking if rebuild required for ~w',[T],SL),
        \+ is_rebuild_required(T,[],SL,Opts),
        !,
        report('~w exists',[T],SL,Opts).
build(T,SL,Opts) :-
        \+ target_bindrule(T,_),
        report('Don\'t know how to make ~w',[T],SL,Opts),
        !,
        fail.
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
        member(D,DL),
        build_count(D,Nd),
        (build_count(T,Nt) -> Nd > Nt; true),
	!,
        report('Target ~w has recently rebuilt dependency ~w - rebuilding',[T,D],SL,Opts).
is_rebuild_required(T,DL,SL,Opts) :-
        \+ exists_directory(T),
        member(D,DL),
        is_built_after(D,T,Opts),
        !,
        report('Target ~w built before dependency ~w - rebuilding',[T,D],SL,Opts).
is_rebuild_required(T,DL,SL,Opts) :-
        member(D,DL),
        \+ exists_target(D,Opts),
        !,
        report('Target ~w has unbuilt dependency ~w - rebuilding',[T,D],SL,Opts).
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


:- dynamic build_count/2.
:- dynamic build_counter/1.

flag_as_rebuilt(T) :-
    next_build_counter(N),
    retractall(build_count(T,_)),
    assert(build_count(T,N)).

next_build_counter(N) :-
    build_counter(Last),
    !,
    N is Last + 1,
    retract(build_counter(Last)),
    assert(build_counter(N)).

next_build_counter(1) :-
    assert(build_counter(1)).

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
	string_chars(Exec,['@'|SilentChars]),
	!,
	string_chars(Silent,SilentChars),
	silent_run_exec(Silent,Opts).
run_exec(Exec,Opts) :-
        report('~w',[Exec],Opts),
	silent_run_exec(Exec,Opts).

silent_run_exec(Exec,Opts) :-
        get_time(T1),
        shell(Exec,Err),
        get_time(T2),
        DT is T2-T1,
        debug_report(build,'  Return: ~w Time: ~w',[Err,DT],Opts),
        Err=0,
        !.

silent_run_exec(Exec,_Opts) :-
        throw(error(run(Exec))).

% ----------------------------------------
% RULES AND PATTERN MATCHING
% ----------------------------------------



target_bindrule(T,rb(T,Ds,Execs)) :-
        mkrule_default(TP1,DP1,Exec1,Goal,Bindings),
        append(Bindings,_,Bindings_Open),
        V=v(_Base,T,Ds,Bindings_Open),
        normalize_patterns(TP1,TPs,V),

        % we allow multiple heads;
        % only one of the specified targets has to match
        member(TP,TPs),
        debug(bindrule,'  pre TP/DPs: ~w / ~w ==> ~w',[TP,DPs,ExecPs]),
        uniq_pattern_match(TP,T),

        normalize_patterns(DP1,DPs,V),
        normalize_patterns(Exec1,ExecPs,V),
        debug(bindrule,'  pst1 TP/DPs: ~w / ~w ==> ~w :: ~w',[TP,DPs,ExecPs,Goal]),

        Goal,
        debug(bindrule,'  pst TP/DPs: ~w / ~w ==> ~w',[TP,DPs,ExecPs]),

        maplist(pattern_target,DPs,ExpandedDeps),
        maplist(split_spaces,ExpandedDeps,DepLists),
	flatten(DepLists,Ds),

        maplist(pattern_exec,ExecPs,ExpandedExecs),
        maplist(split_newlines,ExpandedExecs,ExecLists),
	flatten(ExecLists,Execs).


% semidet
uniq_pattern_match(TL,A) :-
        debug(bindrule,'Matching: ~w to ~w',[TL,A]),
        pattern_match(TL,A),
        debug(bindrule,' Matched: ~w to ~w',[TL,A]),
        !.
uniq_pattern_match(TL,A) :-
        debug(bindrule,' NO_MATCH: ~w to ~w',[TL,A]),
        fail.

pattern_target(TL,A) :- unwrap_t(TL,A).
pattern_exec(TL,A) :- unwrap_t(TL,A).

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
:- user:op(1104,xfy,=*).

consult_gnu_makefile(F) :-
        ensure_loaded(library(plmake/gnumake_parser)),
        parse_gnu_makefile(F,M),
        forall(member(C,M), add_gnumake_clause(C)).

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


add_cmdline_assignment((Var = X)) :-
        global_unbind(Var),
        assert(global_cmdline_binding(Var,X)),
        debug(makeprog,'cmdline assign: ~w = ~w',[Var,X]).

add_gnumake_clause(rule(Ts,Ds,Es)) :-
    add_spec_clause((Ts <-- Ds,Es),[]).

add_gnumake_clause(assignment(Var,"=",Val)) :-
    add_spec_clause((Var = Val)).

add_gnumake_clause(assignment(Var,"?=",Val)) :-
    add_spec_clause((Var ?= Val)).

add_gnumake_clause(assignment(Var,":=",Val)) :-
    add_spec_clause((Var := Val)).

add_gnumake_clause(assignment(Var,"+=",Val)) :-
    add_spec_clause((Var += Val)).

add_gnumake_clause(assignment(Var,"!=",Val)) :-
    add_spec_clause((Var =* Val)).

add_gnumake_clause(C) :-
    format("Error translating ~w~n",[C]).


is_assignment_op(=).
is_assignment_op(?=).
is_assignment_op(:=).
is_assignment_op(+=).
is_assignment_op(=*).

add_spec_clause(Ass) :-
	Ass =.. [Op,Var,_],
	is_assignment_op(Op),
	add_spec_clause(Ass, [Var=Var]).

add_spec_clause( (Var ?= X) ,_VNs) :-
        global_binding(Var,Oldval),
        !,
        debug(makeprog,"Ignoring ~w = ~w since ~w is already bound to ~w",[Var,X,Var,Oldval]).

add_spec_clause( (Var ?= X) ,VNs) :-
        add_spec_clause((Var = X),VNs).

add_spec_clause( Ass ,_VNs) :-
	Ass =.. [Op,Var,X],
	is_assignment_op(Op),
        global_cmdline_binding(Var,Oldval),
        !,
        debug(makeprog,"Ignoring ~w ~w ~w since ~w was bound to ~w on the command-line",[Var,Op,X,Var,Oldval]).

add_spec_clause( (Var = X) ,VNs) :-
	!,
        member(Var=Var,VNs),
        global_unbind(Var),
        assert(global_lazy_binding(Var,X)),
        debug(makeprog,'assign: ~w = ~w',[Var,X]).

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

add_spec_clause( (Var =* X) ,VNs) :-
        !,
        member(Var=Var,VNs),
	shell_eval_str(X,Y),
	!,
        global_unbind(Var),
        assert(global_lazy_binding(Var,Y)),
        debug(makeprog,'assign: ~w =* ~w  ==>  ~w',[Var,X,Y]).

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

expand_vars(X,Y) :-
	expand_vars(X,Y,v("","","",[])).

expand_vars(X,Y,V) :-
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
% Mostly it is a royal pain in the butt, but it seems to be some kind of a marker for pattern evaluation.
% Anyway...
% wrap_t is a construct from cmungall's original code, abstracted into a separate term by me (ihh).
%  Beyond the definition here, I really don't know the original intent here.
% unwrap_t flattens a list into a string, removing any t(...) wrappers in the process,
% and evaluating any postponed functions wrapped with a call(...) compound clause.
wrap_t(t([L]),L) :- member(t(_),L), !.
wrap_t(X,[X]).

%unwrap_t(_,_) :- backtrace(20), fail.
unwrap_t(X,"") :- var(X), !.
unwrap_t(Call,Flat) :- nonvar(Call), Call =.. [call,_|_], !, unwrap_t_call(Call,F), unwrap_t(F,Flat).
unwrap_t(t(X),Flat) :- unwrap_t(X,Flat), !.
unwrap_t([],"") :- !.
unwrap_t([L|Ls],Flat) :- unwrap_t(L,F), unwrap_t(Ls,Fs), string_concat(F,Fs,Flat), !.
unwrap_t(N,A) :- number(A), atom_number(A,N), !.
unwrap_t(A,F) :- atom(A), atom_string(A,F), !.
unwrap_t(S,S) :- string(S), !.
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
        phrase(toks(Toks,V),Chars),
        debug(pattern,'PARSED: ~w ==> ~w',[X,Toks]),
%	backtrace(20),
        !.

toks([],_) --> [].
toks([Tok|Toks],V) --> tok(Tok,V),!,toks(Toks,V).
tok(Var,V) --> ['%'],!,{bindvar_debug('%',V,Var)}.
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
varlabel('<') --> ['(','<',')'],!.
varlabel('*') --> ['(','*',')'],!.
varlabel('@') --> ['(','@',')'],!.
varlabel('^') --> ['(','^',')'],!.
varlabel('^') --> ['(','+',')'],!.
varlabel('^') --> ['(','?',')'],!.
varlabel('*F') --> ['(','*','F',')'],!.
varlabel('*D') --> ['(','*','D',')'],!.
varlabel('@F') --> ['(','@','F',')'],!.
varlabel('@D') --> ['(','@','D',')'],!.
varlabel('<F') --> ['(','<','F',')'],!.
varlabel('<D') --> ['(','<','D',')'],!.
varlabel('^F') --> ['(','^','F',')'],!.
varlabel('^D') --> ['(','^','D',')'],!.
varlabel('^F') --> ['(','+','F',')'],!.
varlabel('^D') --> ['(','+','D',')'],!.
varlabel('^F') --> ['(','?','F',')'],!.
varlabel('^D') --> ['(','?','D',')'],!.
varlabel(A) --> makefile_var_char(C), {atom_chars(A,[C])}.
varlabel(A) --> ['('],makefile_var_atom_from_chars(A),[')'].

bindvar(VL,v(S,T,D,BL),X) :- (var(T); T \= ""), bindauto(VL,v(S,T,D,BL),X), !.  % don't use bindauto when target is bound to null (i.e. first pass through GNU Makefile)
bindvar(VL,v(_,_,_,_),X) :- global_cmdline_binding(VL,X),!.
bindvar(VL,v(_,_,_,_),X) :- global_simple_binding(VL,X),!.
bindvar(VL,v(V1,V2,V3,BL),X) :-
	global_lazy_binding(VL,Y),
	append(BL,[VL=VL],BL2),
	normalize_pattern(Y,Z,v(V1,V2,V3,BL2)),
	unwrap_t(Z,X),
	!.
bindvar(VL,v(_,_,_,BL),X) :- member(VL=X,BL),!.
bindvar(_,v(_,T,_,_),'') :- (var(T); T \= null), !.  % default: bind to empty string, but only if target is bound to null (i.e. not on first pass through GNU Makefile)

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
    
