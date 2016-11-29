% * -*- Mode: Prolog -*- */

:- module(parser_utils,
          [
	   string_from_codes/4,
	   atom_from_codes/4,
	   code_list/4,
	   string_from_chars/4,
	   atom_from_chars/4,
	   char_list/4,
	   whitespace/2,
	   opt_whitespace/2,
	   blank_line/2,
	   alphanums/3,
	   alphanum/3,
	   concat_string_list/2,
	   concat_string_list/3,
	   split_spaces/2,
	   last_element/2,
	   nth_element/3,
	   slice/4
	  ]).

string_from_codes(S,XS) --> {string_codes(XS,XL)}, code_list(C,XL), {C\=[], string_codes(S,C)}.
atom_from_codes(S,XS) --> {string_codes(XS,XL)}, code_list(C,XL), {C\=[], atom_codes(S,C)}.

code_list([C|Cs],XL) --> ['\\'], [C], !, code_list(Cs,XL).
code_list([C|Cs],XL) --> [C], {forall(member(X,XL),C\=X)}, !, code_list(Cs,XL).
code_list([],_) --> [].

string_from_chars(S,XS) --> {string_chars(XS,XL)}, char_list(C,XL), {C\=[], string_chars(S,C)}.
atom_from_chars(S,XS) --> {string_chars(XS,XL)}, char_list(C,XL), {C\=[], atom_chars(S,C)}.

char_list([C|Cs],XL) --> ['\\'], [C], !, char_list(Cs,XL).
char_list([C|Cs],XL) --> [C], {forall(member(X,XL),C\=X)}, !, char_list(Cs,XL).
char_list([],_) --> [].

whitespace --> " ", !, opt_whitespace.
whitespace --> "\t", !, opt_whitespace.

opt_whitespace --> whitespace.
opt_whitespace --> !.

blank_line --> opt_whitespace, "\n", !.

alphanums([X|Xs]) --> alphanum(X),!,alphanums(Xs).
alphanums([]) --> [].
alphanum(X) --> [X],{X@>='a',X@=<'z'},!.
alphanum(X) --> [X],{X@>='A',X@=<'Z'},!.
alphanum(X) --> [X],{X@>='0',X@=<'9'},!.    % foo('0') %
alphanum('_') --> ['_'].

concat_string_list(L,S) :- concat_string_list(L,S,"").
concat_string_list([],"",_).
concat_string_list([S],S,_).
concat_string_list([L|Ls],F,Sep) :- concat_string_list(Ls,R,Sep), string_concat(L,Sep,Lsep), string_concat(Lsep,R,F).

split_spaces(S,L) :-
	split_string(S," "," ",L).

last_element([],"").
last_element([X],X).
last_element([_|Ls],X) :- last_element(Ls,X).

nth_element(_,[],"").
nth_element(1,[X|_],X).
nth_element(N,[_|Ls],X) :- Np is N - 1, nth_element(Np,Ls,X).

slice(_S,_E,[],[]).
slice(1,E,[L|Ls],[L|Rs]) :- E > 0, En is E - 1, slice(1,En,Ls,Rs).
slice(S,E,[_L|Ls],R) :- Sn is S - 1, En is E - 1, slice(Sn,En,Ls,R).
