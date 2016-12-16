% * -*- Mode: Prolog -*- */

:- module(functions,
          [
           makefile_function/3,
           makefile_function/4,
           makefile_subst_ref/3,
           makefile_subst_ref/4,
	   makefile_computed_var/3,
	   makefile_computed_var/4,
	   eval_var/2,
	   eval_var/3
           ]).

:- use_module(library(readutil)).

:- use_module(library(biomake/utils)).
:- use_module(library(biomake/embed)).
:- use_module(library(biomake/biomake)).

makefile_function(Result) --> makefile_function(Result,v(null,null,null,[])).

makefile_function(Result,V) --> lb("subst"), xchr_arg(From,V), comma, xchr_arg(To,V), comma, xchr_arg(Src,V), rb, !,
	{ phrase(subst(From,To,Rc),Src),
	  string_chars(Result,Rc) }.

makefile_function(Result,V) --> lb("patsubst"), xchr_arg(From,V), comma, xchr_arg(To,V), comma, xlst_arg(Src,V), rb, !,
	{ phrase(patsubst_lr(FL,FR),From),
	  phrase(patsubst_lr(TL,TR),To),
	  patsubst_all(FL,FR,TL,TR,Src,R),
	  concat_string_list_spaced(R,Result) }.

makefile_function(Result,V) --> lb("strip"), xlst_arg(L,V), rb, !,
	{ concat_string_list_spaced(L,Result) }.

makefile_function(Result,V) --> lb("findstring"), xstr_arg(S,V), comma, xlst_arg(L,V), rb, !,
	{ findstring(S,L,Result) }.

makefile_function(Result,V) --> lb("filter"), xchr_arg(P,V), comma, xlst_arg(L,V), rb, !,
	{ phrase(patsubst_lr(PL,PR),P),
	  filter(PL,PR,L,R),
	  concat_string_list_spaced(R,Result) }.

makefile_function(Result,V) --> lb("filter-out"), xchr_arg(P,V), comma, xlst_arg(L,V), rb, !,
	{ phrase(patsubst_lr(PL,PR),P),
	  filter_out(PL,PR,L,R),
	  concat_string_list_spaced(R,Result) }.

makefile_function(Result,V) --> lb("sort"), xlst_arg(L,V), rb, !,
	{ sort(L,S),
	  remove_dups(S,R),
	  concat_string_list_spaced(R,Result) }.

makefile_function(Result,V) --> lb("word"), xnum_arg(N,V), comma, xlst_arg(L,V), rb, !,
	{ nth_element(N,L,Result) }.

makefile_function(Result,V) --> lb("wordlist"), xnum_arg(S,V), comma, xnum_arg(E,V), comma, xlst_arg(L,V), rb, !,
	{ slice(S,E,L,Sliced),
	  concat_string_list(Sliced,Result," ") }.

makefile_function(Result,V) --> lb("words"), xlst_arg(L,V), rb, !,
	{ length(L,Result) }.

makefile_function(Result,V) --> lb("firstword"), xlst_arg([Result|_],V), rb, !.

makefile_function(Result,V) --> lb("lastword"), xlst_arg(L,V), rb, !,
	{ last_element(L,Result) }.

makefile_function(Result,V) --> lb("dir"), xlst_arg(Paths,V), rb, !,
	{ maplist(file_directory_slash,Paths,R),
	  concat_string_list_spaced(R,Result) }.

makefile_function(Result,V) --> lb("notdir"), xlst_arg(Paths,V), rb, !,
	{ maplist(file_base_name,Paths,R),
	  concat_string_list_spaced(R,Result) }.

makefile_function(Result,V) --> lb("basename"), xlst_arg(Paths,V), rb, !,
	{ maplist(basename,Paths,R),
	  concat_string_list_spaced(R,Result) }.

makefile_function(Result,V) --> lb("suffix"), xlst_arg(Paths,V), rb, !,
	{ maplist(suffix,Paths,R),
	  concat_string_list_spaced(R,Result) }.

makefile_function(Result,V) --> lb("addsuffix"), str_arg(Suffix), comma, xlst_arg(Prefixes,V), rb, !,
	{ addsuffix(Suffix,Prefixes,R),
	  concat_string_list_spaced(R,Result) }.

makefile_function(Result,V) --> lb("addprefix"), str_arg(Prefix), comma, xlst_arg(Suffixes,V), rb, !,
	{ addprefix(Prefix,Suffixes,R),
	  concat_string_list_spaced(R,Result) }.

makefile_function(Result,V) --> lb("join"), xlst_arg(Prefixes,V), comma, xlst_arg(Suffixes,V), rb, !,
	{ maplist(string_concat,Prefixes,Suffixes,R),
	  concat_string_list_spaced(R,Result) }.

makefile_function(Result,V) --> lb("wildcard"), xstr_arg(W,V), rb, !,
	{ expand_file_name(W,R),
	  concat_string_list_spaced(R,Result) }.

makefile_function(Result,V) --> lb("abspath"), xstr_arg(Path,V), rb, !,
        { absolute_file_name(Path,Result); Result = "" }.

makefile_function(Result,V) --> lb("realpath"), xstr_arg(Path,V), rb, !,
        { (absolute_file_name(Path,Result), (exists_file(Result); exists_directory(Result))); Result = "" }.

makefile_function(Result,V) --> lb("call"), xvar_arg(UserFunc,V), opt_whitespace, call_param_list(L,V), rb, !,
        { V = v(V1,V2,V3,BLold),
	  call_bindings(L,1,BLnew),
	  append(BLold,BLnew,BL),
	  eval_var(UserFunc,Result,v(V1,V2,V3,BL)) }.

makefile_function(Result,V) --> lb("shell"), xstr_arg(Exec,V), rb, !,
	{ shell_eval_str(Exec,Result) }.

makefile_function(Result,V) --> lb("foreach"), var_arg(Var), opt_whitespace, comma, xlst_arg(List,V), comma, str_arg(Text), rb, !,
        { makefile_foreach(Var,List,Text,R,V),
	  concat_string_list_spaced(R,Result) }.

makefile_function(Result,V) --> lb("if"), xstr_arg(Condition,V), opt_whitespace, comma, str_arg(Then), comma, str_arg(Else), rb, !,
        { Condition = "" -> expand_vars(Else,Result,V); expand_vars(Then,Result,V) }.

makefile_function(Result,V) --> lb("if"), xstr_arg(Condition,V), opt_whitespace, comma, str_arg(Then), rb, !,
        { Condition = "" -> Result = ""; expand_vars(Then,Result,V) }.

makefile_function(Result,V) --> lb("or"), opt_whitespace, cond_param_list(L), rb, !,
        { makefile_or(L,Result,V) }.

makefile_function(Result,V) --> lb("and"), opt_whitespace, cond_param_list(L), rb, !,
        { makefile_and(L,Result,V) }.

makefile_function(Result,_V) --> lb("value"), opt_whitespace, var_arg(Var), rb,
        { atom_string(VarAtom,Var), global_binding(VarAtom,Result) }, !.

makefile_function(Result,V) --> lb("value"), opt_whitespace, var_arg(Var), rb, !,
        { bindvar(Var,V,Result) }.

makefile_function(Result,V) --> lb("iota"), opt_whitespace, xnum_arg(N,V), rb, !,
	{ iota(N,L),
 	  concat_string_list_spaced(L,Result) }.

makefile_function(Result,V) --> lb("iota"), opt_whitespace, xnum_arg(S,V), comma, opt_whitespace, xnum_arg(E,V), rb, !,
	{ iota(S,E,L),
	  concat_string_list_spaced(L,Result) }.

makefile_function(Result,V) --> lb("add"), opt_whitespace, xstr_arg(Na,V), comma, opt_whitespace, xlst_arg(List,V), rb, !,
        { maplist(add(Na),List,ResultList),
	  concat_string_list_spaced(ResultList,Result) }.

makefile_function(Result,V) --> lb("multiply"), opt_whitespace, xstr_arg(Na,V), comma, opt_whitespace, xlst_arg(List,V), rb, !,
        { maplist(multiply(Na),List,ResultList),
	  concat_string_list_spaced(ResultList,Result) }.

makefile_function(Result,V) --> lb("divide"), opt_whitespace, xstr_arg(Na,V), comma, opt_whitespace, xlst_arg(List,V), rb, !,
        { maplist(divide(Na),List,ResultList),
	  concat_string_list_spaced(ResultList,Result) }.

makefile_function(Result,_V) --> lb("bagof"), str_arg(Template), comma, str_arg(Goal), rb, !,
	{ eval_bagof(Template,Goal,Result) }.

makefile_function("",_V) --> ['('], str_arg(S), [')'], !, {format("Warning: unknown function $(~w)~n",[S])}.

makefile_subst_ref(Result) --> makefile_subst_ref(Result,v(null,null,null,[])).

makefile_subst_ref(Result,V) --> ['('], xvar_arg(Var,V), [':'], suffix_arg(From), ['='], suffix_arg(To), [')'], !,
	{ eval_var(Var,Val,V),
	  string_chars(Val,Vc),
	  phrase(patsubst_lr(FL,FR),['%'|From]),
	  phrase(patsubst_lr(TL,TR),['%'|To]),
	  patsubst(FL,FR,TL,TR,Vc,Rc),
	  string_chars(Result,Rc) }.

makefile_computed_var(Result) --> makefile_computed_var(Result,v(null,null,null,[])).

makefile_computed_var(Result,V) --> ['('], xvar_arg(Var,V), [')'], !,
	{ eval_var(Var,Result,V) }.

lb(Func) --> ['('], {string_chars(Func,Cs)}, opt_whitespace, Cs, [' '], !.
rb --> opt_whitespace, [')'].

comma --> opt_whitespace, [','].
xlst_arg(L,V) --> xstr_arg(S,V), !, {split_spaces(S,L)}.
xchr_arg(C,V) --> xstr_arg(S,V), !, {string_chars(S,C)}.
xnum_arg(N,V) --> xstr_arg(S,V), !, {atom_number(S,N)}.
xstr_arg(Sx,V) --> str_arg(S), !, {expand_vars(S,Sx,V)}.
chr_arg(C) --> str_arg(S), !, {string_chars(S,C)}.
str_arg(S) --> opt_whitespace, str_arg_outer(S).
str_arg_outer(S) --> ['('], !, str_arg_inner(Si), [')'], str_arg_outer(Rest), {concat_string_list(["(",Si,")",Rest],S)}.
str_arg_outer(S) --> string_from_chars(Start,"(),"), !, str_arg_outer(Rest), {string_concat(Start,Rest,S)}.
str_arg_outer("") --> !.
str_arg_inner(S) --> ['('], !, str_arg_inner(Si), [')'], str_arg_inner(Rest), {concat_string_list(["(",Si,")",Rest],S)}.
str_arg_inner(S) --> string_from_chars(Start,"()"), !, str_arg_inner(Rest), {string_concat(Start,Rest,S)}.
str_arg_inner("") --> !.

xvar_arg(S,_V) --> var_arg(S).
xvar_arg(S,V) --> ['$','('], !, xstr_arg(X,V), [')'], {eval_var(X,S,V)}.

var_arg(S) --> opt_whitespace, makefile_var_string_from_chars(S).

suffix_arg(C) --> char_list(C,['=',')',' ']).

var_expr(VarName,Expr) :-
	concat_string_list(["$(",VarName,")"],Expr).

eval_var(VarName,Val) :-
	var_expr(VarName,Expr),
	expand_vars(Expr,Val).

eval_var(VarName,Val,V) :-
	var_expr(VarName,Expr),
	expand_vars(Expr,Val,V).

call_param_list([],_V) --> [].
call_param_list([P|Ps],V) --> comma, !, xstr_arg(P,V), call_param_list(Ps,V).

call_bindings([],_,[]).
call_bindings([Param|Params],Num,[NumAtom=Param|Vars]) :-
	atom_number(NumAtom,Num),
	NextNum is Num + 1,
	call_bindings(Params,NextNum,Vars).

makefile_foreach(_,[],_,[],_).
makefile_foreach(Var,[L|Ls],Text,[R|Rs],V) :-
    atom_string(VarAtom,Var),
    V = v(V1,V2,V3,BLold),
    append(BLold,[VarAtom=L],BL),
    expand_vars(Text,R,v(V1,V2,V3,BL)),
    makefile_foreach(Var,Ls,Text,Rs,V).

cond_param_list([P|Ps]) --> str_arg(P), comma, !, cond_param_list(Ps).
cond_param_list([P]) --> str_arg(P), !.

makefile_or([],"",_) :- !.
makefile_or([C|_],Result,V) :- expand_vars(C,Result,V), Result \= "", !.
makefile_or([_|Cs],Result,V) :- makefile_or(Cs,Result,V).

makefile_and([C],Result,V) :- expand_vars(C,Result,V), !.
makefile_and([C|Cs],Result,V) :- expand_vars(C,X,V), X \= "", !, makefile_and(Cs,Result,V).
makefile_and(_,"",_).

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

iota(N,L) :- iota(1,N,L).
iota(S,E,[]) :- S > E, !.
iota(S,E,[S|L]) :- Snext is S + 1, iota(Snext,E,L).

% these arithmetic functions are highly idiosyncratic to this module - do not re-use!
multiply(Aa,Bs,C) :- atom_string(Aa,As), number_string(A,As), number_string(B,Bs), C is A * B.
divide(Aa,Bs,C) :- atom_string(Aa,As), number_string(A,As), number_string(B,Bs), C is B / A.
add(Aa,Bs,C) :- atom_string(Aa,As), number_string(A,As), number_string(B,Bs), C is A + B.
