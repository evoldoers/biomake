% * -*- Mode: Prolog -*- */

:- module(functions,
          [
           makefile_function/4,
           makefile_subst_ref/4,
	   makefile_computed_var/4
           ]).

:- use_module(library(plmake/utils)).
:- use_module(library(plmake/plmake)).

makefile_function(Result,V) --> lb("subst"), xchr_arg(From,V), comma, xchr_arg(To,V), comma, xchr_arg(Src,V), rb, !,
	{ phrase(subst(From,To,Rc),Src),
	  string_chars(Result,Rc) }.

makefile_function(Result,V) --> lb("patsubst"), chr_arg(From), comma, chr_arg(To), comma, xlst_arg(Src,V), rb, !,
	{ phrase(patsubst_lr(FL,FR),From),
	  phrase(patsubst_lr(TL,TR),To),
	  patsubst_all(FL,FR,TL,TR,Src,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result,V) --> lb("strip"), xlst_arg(L,V), rb, !,
	{ concat_string_list(L,Result," ") }.

makefile_function(Result,V) --> lb("findstring"), xstr_arg(S,V), comma, xlst_arg(L,V), rb, !,
	{ findstring(S,L,Result) }.

makefile_function(Result,V) --> lb("filter"), chr_arg(P), comma, xlst_arg(L,V), rb, !,
	{ phrase(patsubst_lr(PL,PR),P),
	  filter(PL,PR,L,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result,V) --> lb("filter-out"), chr_arg(P), comma, xlst_arg(L,V), rb, !,
	{ phrase(patsubst_lr(PL,PR),P),
	  filter_out(PL,PR,L,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result,V) --> lb("sort"), xlst_arg(L,V), rb, !,
	{ sort(L,S),
	  remove_dups(S,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result,V) --> lb("word"), num_arg(N), comma, xlst_arg(L,V), rb, !,
	{ nth_element(N,L,Result) }.

makefile_function(Result,V) --> lb("wordlist"), num_arg(S), comma, num_arg(E), comma, xlst_arg(L,V), rb, !,
	{ slice(S,E,L,Sliced),
	  concat_string_list(Sliced,Result," ") }.

makefile_function(Result,V) --> lb("words"), xlst_arg(L,V), rb, !,
	{ length(L,Result) }.

makefile_function(Result,V) --> lb("firstword"), xlst_arg([Result|_],V), rb, !.

makefile_function(Result,V) --> lb("lastword"), xlst_arg(L,V), rb, !,
	{ last_element(L,Result) }.

makefile_function(Result,V) --> lb("dir"), xstr_arg(Path,V), rb, !,
	{ file_directory_name(Path,D),
	  string_concat(D,"/",Result) }.  % GNU make adds the trailing '/'

makefile_function(Result,V) --> lb("notdir"), xstr_arg(Path,V), rb, !,
	{ file_base_name(Path,Result) }.

makefile_function(Result,V) --> lb("basename"), xlst_arg(Paths,V), rb, !,
	{ maplist(basename,Paths,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result,V) --> lb("suffix"), xlst_arg(Paths,V), rb, !,
	{ maplist(suffix,Paths,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result,V) --> lb("addsuffix"), xstr_arg(Suffix,V), comma, xlst_arg(Prefixes,V), rb, !,
	{ addsuffix(Suffix,Prefixes,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result,V) --> lb("addprefix"), xstr_arg(Prefix,V), comma, xlst_arg(Suffixes,V), rb, !,
	{ addprefix(Prefix,Suffixes,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result,V) --> lb("join"), xlst_arg(Prefixes,V), comma, xlst_arg(Suffixes,V), rb, !,
	{ maplist(string_concat,Prefixes,Suffixes,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result,V) --> lb("wildcard"), xstr_arg(W,V), rb, !,
	{ expand_file_name(W,R),
	  concat_string_list(R,Result," ") }.

makefile_function(Result,V) --> lb("abspath"), xstr_arg(Path,V), rb, !,
        { absolute_file_name(Path,Result); Result = "" }.

makefile_function(Result,V) --> lb("realpath"), xstr_arg(Path,V), rb, !,
        { (absolute_file_name(Path,Result), (exists_file(Result); exists_directory(Result))); Result = "" }.

makefile_function("",_V) --> ['('], whitespace, str_arg(S), [')'], !, {format("Warning: unknown function ~w~n",[S])}.
makefile_function("",_V) --> ['('], str_arg(S), whitespace, [')'], !, {format("Warning: unknown function ~w~n",[S])}.

makefile_subst_ref(Result,V) --> ['('], var_arg(Var), [':'], suffix_arg(From), ['='], suffix_arg(To), [')'], !,
	{ concat_string_list(["$(",Var,")"],VarExpr),
	  expand_vars(VarExpr,Val,V),
	  string_chars(Val,Vc),
	  phrase(patsubst_lr(FL,FR),['%'|From]),
	  phrase(patsubst_lr(TL,TR),['%'|To]),
	  patsubst(FL,FR,TL,TR,Vc,Rc),
	  string_chars(Result,Rc) }.

makefile_computed_var(Result,V) --> ['('], xstr_arg(Var,V), [')'], !,
	{ concat_string_list(["$(",Var,")"],Expr),
	  expand_vars(Expr,Result,V) }.

lb(Func) --> ['('], {string_chars(Func,Cs)}, opt_whitespace, Cs, [' '], !.
rb --> opt_whitespace, [')'].

comma --> opt_whitespace, [','].
xlst_arg(L,V) --> xstr_arg(S,V), !, {split_spaces(S,L)}.
xchr_arg(C,V) --> xstr_arg(S,V), !, {string_chars(S,C)}.
xstr_arg(Sx,V) --> str_arg(S), !, {expand_vars(S,Sx,V)}.
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

basename_suffix(B,S) --> string_from_chars(B," "), ['.'], string_from_chars(Srest," ."), {string_concat(".",Srest,S)}, !.
basename_suffix("",S) --> ['.'], string_from_chars(Srest," ."), {string_concat(".",Srest,S)}, !.
basename_suffix(B,"") --> string_from_chars(B," .").

basename(P,B) :- string_chars(P,Pc), phrase(basename_suffix(B,_),Pc).
suffix(P,S) :- string_chars(P,Pc), phrase(basename_suffix(_,S),Pc).

addsuffix(_,[],[]).
addsuffix(S,[N|Ns],[R|Rs]) :- string_concat(N,S,R), addsuffix(S,Ns,Rs).

addprefix(_,[],[]).
addprefix(P,[N|Ns],[R|Rs]) :- string_concat(P,N,R), addprefix(P,Ns,Rs).

