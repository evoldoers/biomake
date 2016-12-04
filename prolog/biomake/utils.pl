% * -*- Mode: Prolog -*- */

:- module(utils,
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
	   n_chars/3,
	   concat_string_list/2,
	   concat_string_list/3,
	   concat_string_list_spaced/2,
	   split_spaces/2,
	   split_newlines/2,
	   last_element/2,
	   nth_element/3,
	   slice/4,
	   shell_path/1,
	   shell_wrap/2,
	   shell_eval/2,
	   shell_eval_str/2,
	   file_directory_slash/2,
	   quote_string/2,
	   newlines_to_spaces/2,
	   to_string/2,
	   equal_as_strings/2,
	   makefile_var_char/3,
	   makefile_var_chars/3,
	   makefile_var_atom_from_chars/3,
	   makefile_var_string_from_chars/3,
	   makefile_var_code/3,
	   makefile_var_codes/3,
	   makefile_var_atom_from_codes/3,
	   makefile_var_string_from_codes/3,
	   biomake_private_filename/3,
	   biomake_private_filename_mkdir/3
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

n_chars(N,_,[]) :- N =< 0, !.
n_chars(N,C,[C|Ls]) :- Ndec is N - 1, n_chars(Ndec,C,Ls), !.

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

shell_path(Path) :-
	expand_file_search_path(path(sh),Path),
	exists_file(Path),
	!.

shell_wrap(Exec,ShellExec) :-
	shell_path(Sh),
	!,
	format(string(ShellExec),"~w -c ~q",[Sh,Exec]).

shell_eval(Exec,CodeList) :-
        process_create(path(sh),['-c',Exec],[stdout(pipe(Stream)),
					     stderr(null),
					     process(Pid)]),
        read_stream_to_codes(Stream,CodeList),
	process_wait(Pid,_Status),
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

quote_string(S,QS) :-
    string_chars(S,Cs),
    phrase(escape_quotes(ECs),Cs),
    append(['"'|ECs],['"'],QCs),
    string_chars(QS,QCs).

escape_quotes([]) --> [].
escape_quotes(['\\','\\'|Cs]) --> ['\\'], !, escape_quotes(Cs).
escape_quotes(['\\','"'|Cs]) --> ['"'], !, escape_quotes(Cs).
escape_quotes([C|Cs]) --> [C], !, escape_quotes(Cs).

to_string(A,S) :- atomics_to_string([A],S).
equal_as_strings(X,Y) :-
	to_string(X,S),
	to_string(Y,S).


% We allow only a restricted subset of characters in variable names,
% compared to the GNU make specification.
% (seriously, does anyone use makefile variable names containing brackets, commas, colons, etc?)
makefile_var_char(C) --> alphanum_char(C).
makefile_var_char('_') --> ['_'].
makefile_var_char('-') --> ['-'].

makefile_var_chars([]) --> [].
makefile_var_chars([C|Cs]) --> makefile_var_char(C), makefile_var_chars(Cs).

makefile_var_atom_from_chars(A) --> makefile_var_chars(Cs), {atom_chars(A,Cs)}.
makefile_var_string_from_chars(S) --> makefile_var_chars(Cs), {string_chars(S,Cs)}.

% define these again as character codes, because Prolog is so annoying
makefile_var_code(C) --> alphanum_code(C).
makefile_var_code(95) --> [95].  % underscore '_'
makefile_var_code(45) --> [45].  % hyphen '-'

makefile_var_codes([]) --> [].
makefile_var_codes([C|Cs]) --> makefile_var_code(C), makefile_var_codes(Cs).

makefile_var_atom_from_codes(A) --> makefile_var_codes(Cs), {atom_codes(A,Cs)}.
makefile_var_string_from_codes(S) --> makefile_var_codes(Cs), {string_codes(S,Cs)}.

biomake_private_dir(Target,Path) :-
	absolute_file_name(Target,F),
	file_directory_name(F,D),
	format(string(Path),"~w/.biomake",[D]).

biomake_private_subdir(Target,Subdir,Path) :-
	biomake_private_dir(Target,Private),
	format(string(Path),"~w/~w",[Private,Subdir]).

biomake_private_filename(Target,Subdir,Filename) :-
	biomake_private_subdir(Target,Subdir,Private),
	absolute_file_name(Target,F),
	file_base_name(F,N),
	format(string(Filename),"~w/~w",[Private,N]).

biomake_private_filename_mkdir(Target,Subdir,Filename) :-
	biomake_private_dir(Target,Path),
	(exists_directory(Path); make_directory(Path)),
	biomake_private_subdir(Target,Subdir,SubPath),
	(exists_directory(SubPath); make_directory(SubPath)),
	biomake_private_filename(Target,Subdir,Filename).
