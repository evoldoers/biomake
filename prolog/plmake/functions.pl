% * -*- Mode: Prolog -*- */

:- module(functions,
          [
           makefile_function/3,
           makefile_subst_ref/3
           ]).

:- use_module(library(plmake/utils)).
:- use_module(library(plmake/plmake)).

makefile_function(Result) --> lb("subst"), xchr_arg(From), comma, xchr_arg(To), comma, xchr_arg(Src), rb, !,
	{ phrase(subst(From,To,Rc),Src),
	  string_chars(Result,Rc) }.

makefile_function(Result) --> lb("patsubst"), chr_arg(From), comma, chr_arg(To), comma, xlst_arg(Src), rb, !,
	{ phrase(patsubst_lr(FL,FR),From),
	  phrase(patsubst_lr(TL,TR),To),
	  patsubst_all(FL,FR,TL,TR,Src,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result) --> lb("strip"), xlst_arg(L), rb, !,
	{ concat_string_list(L,Result," ") }.

makefile_function(Result) --> lb("findstring"), xstr_arg(S), comma, xlst_arg(L), rb, !,
	{ findstring(S,L,Result) }.

makefile_function(Result) --> lb("filter"), chr_arg(P), comma, xlst_arg(L), rb, !,
	{ phrase(patsubst_lr(PL,PR),P),
	  filter(PL,PR,L,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result) --> lb("filter-out"), chr_arg(P), comma, xlst_arg(L), rb, !,
	{ phrase(patsubst_lr(PL,PR),P),
	  filter_out(PL,PR,L,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result) --> lb("sort"), xlst_arg(L), rb, !,
	{ sort(L,S),
	  remove_dups(S,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result) --> lb("word"), num_arg(N), comma, xlst_arg(L), rb, !,
	{ nth_element(N,L,Result) }.

makefile_function(Result) --> lb("wordlist"), num_arg(S), comma, num_arg(E), comma, xlst_arg(L), rb, !,
	{ slice(S,E,L,Sliced),
	  concat_string_list(Sliced,Result," ") }.

makefile_function(Result) --> lb("words"), xlst_arg(L), rb, !,
	{ length(L,Result) }.

makefile_function(Result) --> lb("firstword"), xlst_arg([Result|_]), rb, !.

makefile_function(Result) --> lb("lastword"), xlst_arg(L), rb, !,
	{ last_element(L,Result) }.

makefile_function("") --> ['('], whitespace, str_arg(S), [')'], !, {format("Warning: unknown function ~w~n",[S])}.
makefile_function("") --> ['('], str_arg(S), whitespace, [')'], !, {format("Warning: unknown function ~w~n",[S])}.

makefile_subst_ref(Result) --> ['('], var_arg(Var), [':'], suffix_arg(From), ['='], suffix_arg(To), [')'], !,
	{ concat_string_list(["$(",Var,")"],VarExpr),
	  expand_vars(VarExpr,Val),
	  string_chars(Val,Vc),
	  phrase(patsubst_lr(FL,FR),['%'|From]),
	  phrase(patsubst_lr(TL,TR),['%'|To]),
	  patsubst(FL,FR,TL,TR,Vc,Rc),
	  string_chars(Result,Rc) }.

lb(Func) --> ['('], {string_chars(Func,Cs)}, opt_whitespace, Cs, [' '], !.
rb --> opt_whitespace, [')'].

comma --> opt_whitespace, [','].
xlst_arg(L) --> xstr_arg(S), !, {split_spaces(S,L)}.
xchr_arg(C) --> xstr_arg(S), !, {string_chars(S,C)}.
xstr_arg(Sx) --> str_arg(S), !, {expand_vars(S,Sx)}.
chr_arg(C) --> str_arg(S), !, {string_chars(S,C)}.
str_arg(S) --> opt_whitespace, str_arg_outer(S).
str_arg_outer(S) --> ['('], !, str_arg_inner(Si), [')'], {concat_string_list(["(",Si,")"],S)}.
str_arg_outer(S) --> string_from_chars(Start,"(),"), !, str_arg_outer(Rest), {string_concat(Start,Rest,S)}.
str_arg_outer("") --> !.
str_arg_inner(S) --> ['('], !, str_arg_inner(Si), [')'], {concat_string_list(["(",Si,")"],S)}.
str_arg_inner(S) --> string_from_chars(Start,"()"), !, str_arg_inner(Rest), {string_concat(Start,Rest,S)}.
str_arg_inner("") --> !.

var_arg(S) --> alphanum(C), alphanums(Cs), !, {string_chars(S,[C|Cs])}.
suffix_arg(C) --> char_list(C,['=',')',' ']).

num_arg(N) --> opt_whitespace, num_chars(C), {C\=[],number_chars(N,C)}.
num_chars([]) --> [].
num_chars([C|Cs]) --> num_char(C), num_chars(Cs).
num_char(X) --> [X],{X@>='0',X@=<'9'},!.    % foo('0') %

subst(Cs,Ds,Result) --> Cs, !, subst(Cs,Ds,Rest), {append(Ds,Rest,Result)}.
subst(Cs,Ds,[C|Rest]) --> [C], !, subst(Cs,Ds,Rest).
subst(_Cs,_Ds,[]) --> !.

patsubst_all(_,_,_,_,[],[]).
patsubst_all(FL,FR,TL,TR,[Src|Srest],[Dest|Drest]) :-
	string_chars(Src,Sc),
	patsubst(FL,FR,TL,TR,Sc,Dc),
	string_chars(Dest,Dc),
	!,
	patsubst_all(FL,FR,TL,TR,Srest,Drest).

patsubst(FL,FR,TL,TR,S,D) :-
	phrase(patsubst_match(FL,FR,Match),S),
	append(TL,Match,DL),
	append(DL,TR,D).
patsubst(_,_,_,_,S,S).

patsubst_lr([],[]) --> [].
patsubst_lr([C|L],R) --> [C], {C\='%'}, !, patsubst_lr(L,R).
patsubst_lr([],R) --> ['%'], patsubst_lr_r(R).
patsubst_lr_r([]) --> [].
patsubst_lr_r([C|R]) --> [C], !, patsubst_lr_r(R).

patsubst_match(L,R,Match) --> L, patsubst_match_m(R,Match).
patsubst_match_m(R,[C|Match]) --> [C], patsubst_match_m(R,Match).
patsubst_match_m(R,[]) --> R.

findstring(S,[L|_],S) :- string_codes(S,Sc), string_codes(L,Lc), Sc = Lc, !.  % this seems a bit contrived, but a straight test for string equality doesn't seem to work
findstring(S,[_|L],R) :- findstring(S,L,R).
findstring(_,[],"").

filter(_,_,[],[]).
filter(L,R,[Src|Srest],[Src|Drest]) :-
	string_chars(Src,Sc),
	phrase(patsubst_match(L,R,_),Sc),
	!,
	filter(L,R,Srest,Drest).
filter(L,R,[_|Srest],Dest) :- filter(L,R,Srest,Dest).

filter_out(_,_,[],[]).
filter_out(L,R,[Src|Srest],Dest) :-
	string_chars(Src,Sc),
	phrase(patsubst_match(L,R,_),Sc),
	!,
	filter_out(L,R,Srest,Dest).
filter_out(L,R,[Src|Srest],[Src|Drest]) :- filter_out(L,R,Srest,Drest).

% remove_dups assumes list is sorted
remove_dups([],[]).
remove_dups([X,X|Xs],Y) :- !, remove_dups([X|Xs],Y).
remove_dups([X|Xs],[X|Ys]) :- remove_dups(Xs,Ys).

