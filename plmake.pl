% * -*- Mode: Prolog -*- */

:- module(plmake,
          [
           build/1,
           build/2,
           consult_buildfile/0,
           consult_buildfile/1,

           target_bindrule/2,
           rule_dependencies/3,
           is_rebuild_required/4
           ]).

/** <module> Prolog implementation of Makefile-inspired build system

  See the README

  */



% ----------------------------------------
% TOP-LEVEL
% ----------------------------------------

%% build(+Target)
%% build(+Target, +Opts:list)
%% build(+Target, +Stack:list,+Opts:list)
%
% builds Target using rules

build(T) :-
        build(T,[]).
build(T,Opts) :-
        build(T,[],Opts).

build(T,SL,Opts) :-
        %report('Target: ~w',[T]),
        target_bindrule(T,Rule),
        debug(build3,'  Bindrule: ~w',[Rule]),
        rule_dependencies(Rule,DL,Opts),
        report('NT: ~w <-- ~w',[T,DL],SL,Opts),
        !,
        build_targets(DL,[T|SL],Opts), % semidet
        (   is_rebuild_required(T,DL,SL,Opts)
        ->  rule_execs(Rule,Execs,Opts),
            run_execs(Execs,Opts)
        ;   true),
        report('NT: ~w is up to date',[T],SL,Opts).
build(T,SL,Opts) :-
        debug(build3,'  checking if rebuild required for ~w',[T]),
        \+ is_rebuild_required(T,[],SL,Opts),
        !,
        report('T: ~w exists',[T],SL,Opts).
build(T,SL,Opts) :-
        report('T: ~w FAILED',[T],SL,Opts),
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

report(Fmt,Args,SL,Opts) :-
        maplist(write_tab(Opts),SL),
        format(Fmt,Args),
        nl.

write_tab(_,_) :- write('    ').

% ----------------------------------------
% DEPENDENCY MANAGEMENT
% ----------------------------------------

is_rebuild_required(_,_,_,Opts) :-
        member(always_make(true),Opts), % TODO
        !.
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
        report('Command (not executing): ~w',[Exec],Opts).
run_exec(Exec,Opts) :-
        report('Command: ~w',[Exec],Opts),
        get_time(T1),
        shell(Exec,Err),
        get_time(T2),
        DT is T2-T1,
        report('  Return: ~w Time: ~w',[Err,DT],Opts),
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
        debug(build3,'  pre TP/DPs: ~w / ~w ==> ~w',[TP,DPs,ExecPs]),
        uniq_pattern_match(TP,T),
        debug(build3,'  pst1 TP/DPs: ~w / ~w ==> ~w :: ~w',[TP,DPs,ExecPs,Goal]),
        Goal,
        debug(build3,'  pst TP/DPs: ~w / ~w ==> ~w',[TP,DPs,ExecPs]),
        maplist(pattern_target,DPs,Ds),
        maplist(toks_exec,ExecPs,Execs).

        

% semidet
uniq_pattern_match(TL,A) :-
        debug(build3,'Matching: ~w to ~w',[TL,A]),
        pattern_match(TL,A),
        debug(build3,' Matched: ~w to ~w',[TL,A]),
        !.
uniq_pattern_match(TL,A) :-
        debug(build3,' NO_MATCH: ~w to ~w',[TL,A]),
        fail.


pattern_target(t(TL),A) :- atomic_list_concat(TL,A).
toks_exec(t(TL),A) :- flatten(TL,TL2),atomic_list_concat(TL2,A).
%toks_exec(c(TL),A) :- atomic_list_concat(TL,A).


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

:- dynamic global_binding/2.

:- user:op(1100,xfy,<--).

/*
consult_makefile(F) :-
        ensure_loaded(gnumake_parser),
        parse_makefile(F,RL),
        forall(member( rule(T,Ds,Cs), RL),
               add_spec_clause( (T <-- Ds, Cs))).
*/

consult_buildfile :- consult_buildfile('makespec.pro').
consult_buildfile(F) :-
        debug(build,'reading: ~w',[F]),
        open(F,read,IO,[]),
        repeat,
        (   at_end_of_stream(IO)
        ->  !
        ;   read_term(IO,Term,[variable_names(VNs),
                               syntax_errors(error),
                               module(plmake)]),
            debug(build3,'adding: ~w',[Term]),
            add_spec_clause(Term,VNs),
            fail),
        debug(build3,'read: ~w',[F]),
        close(IO).

add_spec_clause( (Var = X,{Goal}) ,VNs) :-
        !,
        normalize_pattern(X,Y,v(_,_,_,VNs)),
        findall(Y,Goal,Ys),
        member(Var=Var,VNs),
        assert(global_binding(Var,Ys)).
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
        Rule =.. [mkrule|_],
        !,
        debug(build3,'with: ~w ~w',[Rule,VNs]),
        assert(with(Rule,VNs)).
add_spec_clause(Term,_) :-
        assert(Term).

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

normalize_patterns(X,X,_) :- var(X),!.
normalize_patterns([],[],_) :- !.
normalize_patterns([P|Ps],[N|Ns],V) :-
        !,
        debug(build3,'*norm: ~w',[P]),
        normalize_pattern(P,N,V),
        normalize_patterns(Ps,Ns,V).
normalize_patterns(P,Ns,V) :-
        normalize_pattern(P,N,V),
        % this is a bit hacky - parsing is too eager to add t(...)
        % wrapper
        (   N=t([L]),member(t(_),L)
        ->  Ns=L
        ;   Ns=[N]).

normalize_pattern(X,X,_) :- var(X),!.
normalize_pattern(t(X),t(X),_) :- !.
normalize_pattern(Term,t(Args),_) :-
        Term =.. [t|Args],!.
normalize_pattern(X,t(Toks),V) :-
        debug(build3,'PARSING: ~w // ~w',[X,V]),
        atom_chars(X,Chars),
        phrase(toks(Toks,V),Chars),
        debug(build3,'PARSED: ~w ==> ~w',[X,Toks]),
        !.

toks([],_) --> [].
toks([Tok|Toks],V) --> tok(Tok,V),!,toks(Toks,V).
tok(Var,V) --> ['%'],!,{bindvar('%',V,Var)}.
tok(Var,V) --> ['$'],varlabel(VL),!,{bindvar(VL,V,Var)}.
tok(Tok,_) --> tok_a(Cs),{atom_chars(Tok,Cs)}.
tok_a([C|Cs]) --> [C],{C\='$',C\='%'},!,tok_a(Cs).
tok_a([]) --> [].
varlabel('<') --> ['<'],!.
varlabel('*') --> ['*'],!.
varlabel('@') --> ['@'],!.
varlabel(A) --> alphanum(C),alphanums(Cs),{atom_chars(A,[C|Cs])}.
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
bindvar(VL,v(_,_,_,_),X) :- global_binding(VL,X),!.
bindvar(VL,v(_,_,_,BL),X) :- debug(build3,'binding ~w= ~w // ~w',[VL,X,BL]),member(VL=X,BL),debug(build3,'bound ~w= ~w',[VL,X]),!.



