% * -*- Mode: Prolog -*- */

:- module(parser_utils,
          [
           show_type/1,
           type_of/2,
	   string_from_codes/4,
	   atom_from_codes/4,
	   code_list/4,
	   string_from_chars/4,
	   atom_from_chars/4,
	   char_list/4,
	   whitespace/2,
	   opt_whitespace/2,
	   space/2,
	   opt_space/2,
	   blank_line/2,
	   alphanum_char/3,
	   alphanum_code/3,
	   concat_string_list/2,
	   concat_string_list/3,
	   concat_string_list_spaced/2,
	   split_spaces/2,
	   split_newlines/2,
	   last_element/2,
	   nth_element/3,
	   slice/4,
	   shell_eval/2,
	   shell_eval_str/2,
	   file_directory_slash/2,
	   newlines_to_spaces/2
	  ]).

string_from_codes(S,XS) --> {string_codes(XS,XL)}, code_list(C,XL), {C\=[], string_codes(S,C)}.
atom_from_codes(S,XS) --> {string_codes(XS,XL)}, code_list(C,XL), {C\=[], atom_codes(S,C)}.

code_list([C|Cs],XL) --> ['\\'], [C], {member(C,XL)}, code_list(Cs,XL).
code_list([C|Cs],XL) --> [C], {forall(member(X,XL),C\=X)}, code_list(Cs,XL).
code_list([],_) --> [].

string_from_chars(S,XS) --> {string_chars(XS,XL)}, char_list(C,XL), {C\=[], string_chars(S,C)}.
atom_from_chars(S,XS) --> {string_chars(XS,XL)}, char_list(C,XL), {C\=[], atom_chars(S,C)}.

char_list([C|Cs],XL) --> ['\\'], [C], {member(C,XL)}, char_list(Cs,XL).
char_list([C|Cs],XL) --> [C], {forall(member(X,XL),C\=X)}, char_list(Cs,XL).
char_list([],_) --> [].

whitespace --> " ", !, opt_whitespace.
whitespace --> "\t", !, opt_whitespace.

opt_whitespace --> whitespace.
opt_whitespace --> !.

space --> " ", !, opt_space.

opt_space --> space.
opt_space --> !.

blank_line --> "\n", !.
blank_line --> space, opt_whitespace, "\n", !.

alphanum_char(X) --> [X],{X@>='A',X@=<'Z'},!.
alphanum_char(X) --> [X],{X@>='a',X@=<'z'},!.
alphanum_char(X) --> [X],{X@>='0',X@=<'9'},!.

alphanum_code(X) --> [X],{X@>=65,X@=<90},!.  % A through Z
alphanum_code(X) --> [X],{X@>=97,X@=<122},!.  % a through z
alphanum_code(X) --> [X],{X@>=48,X@=<57},!.  % 0 through 9

concat_string_list_spaced(L,S) :- concat_string_list(L,S," ").
concat_string_list(L,S) :- concat_string_list(L,S,"").
concat_string_list([],"",_).
concat_string_list([S],S,_).
concat_string_list([L|Ls],F,Sep) :- concat_string_list(Ls,R,Sep), string_concat(L,Sep,Lsep), string_concat(Lsep,R,F).

split_spaces(S,L) :-
	split_string(S," "," ",L).

split_newlines(S,L) :-
	split_string(S,"\n","\n",L).

last_element([],"").
last_element([X],X).
last_element([_|Ls],X) :- last_element(Ls,X).

nth_element(_,[],"").
nth_element(1,[X|_],X).
nth_element(N,[_|Ls],X) :- Np is N - 1, nth_element(Np,Ls,X).

slice(_S,_E,[],[]).
slice(1,E,[L|Ls],[L|Rs]) :- E > 0, En is E - 1, slice(1,En,Ls,Rs).
slice(S,E,[_L|Ls],R) :- Sn is S - 1, En is E - 1, slice(Sn,En,Ls,R).

show_type(X) :- type_of(X,T), format("Type of ~w is ~w.~n",[X,T]).
type_of(X,"var") :- var(X), !.
type_of(X,"integer") :- integer(X), !.
type_of(X,"float") :- float(X), !.
type_of(X,"rational") :- rational(X), !.
type_of(X,"number") :- number(X), !.  % should never be reached
type_of(X,"string") :- string(X), !.
type_of(X,"compound") :- compound(X), !.
type_of(X,"atom") :- atom(X), !.
type_of(_,"unknown").

shell_eval(Exec,CodeList) :-
	process_create(path(sh),['-c',Exec],[stdout(pipe(Stream))]),
        read_stream_to_codes(Stream,CodeList),
        close(Stream).

shell_eval_str(Exec,Result) :-
        shell_eval(Exec,Rnl),
	newlines_to_spaces(Rnl,Rspc),
	string_codes(Result,Rspc).

newlines_to_spaces([],[]).
newlines_to_spaces([10|N],[32|S]) :- newlines_to_spaces(N,S).
newlines_to_spaces([C|N],[C|S]) :- newlines_to_spaces(N,S).

file_directory_slash(Path,Result) :-
	file_directory_name(Path,D),
	string_concat(D,"/",Result).  % GNU make adds the trailing '/'
