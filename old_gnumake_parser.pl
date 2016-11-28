% * -*- Mode: Prolog -*- */

:- module(gnumake_parser,
          [parse_makefile/1,
           translate_makefile/1]).

translate_makefile(File) :-
        parse_makefile(File,DL),
        write_makespec(DL).

parse_makefile(File,DL) :-
        read_file_to_codes(File,Codes,[]),
        atom_codes(A,Codes),
        atom_chars(A,Cs),
        phrase(directives(DL),Cs),

write_makespec(DL) :- 
        maplist(write_directive,DL).

write_directive( rule(T,Ds,[C]) ) :-
        !,
        format('~q <-- ~q,~n  ~q.~n~n',[T,Ds,C]).
write_directive( rule(T,Ds,Cs) ) :-
        !,
        format('~q <-- ~q,~n  ~q.~n~n',[T,Ds,Cs]).
write_directive( X) :- writeln(X).


directives([X|Xs]) -->
        ws,
        directive(X),
        !,
        directives(Xs).
directives([]) --> ws.

directive(X) --> rule(X),!.
directive(X) --> assignment(X),!.
directive(X) --> comment(X).

assignment(X=Y) --> toks(X),spcs,['='],spcs,toks(Y,['\n']).
comment('') --> ['#'],toks(_,['\n']).

        
rule( rule(T,Ds,Cs) ) -->
        target(T), [':'], !, spcs, dependencies(Ds), spcs, nl,
        commands(Cs).

target(T) --> toks(T,[':',' ','\t']),spcs.

dependencies([D|Ds]) --> spcs, dependency(D), !, dependencies(Ds).
dependencies([]) --> [].

dependency(D) --> toks(D).

commands([C|Cs]) --> command(C),!,commands(Cs).
commands([]) --> [].

command(C) --> tabchar,toks(C,['\n']).

toks(A) --> toks(A,[':',' ','\t','\n']).
toks(A,XL) --> clist(CL,XL),{CL\=[],atom_chars(A,CL)}.
clist([C|Cs],XL) --> [C],{forall(member(X,XL),C\=X)},!,clist(Cs,XL).
clist([C|Cs],XL) --> ['\\'],[C],!,clist(Cs,XL).
clist([],_) --> [].


nl --> ['\n'].
tabchar --> ['\t'].
spcs --> [' '],!,spcs.
spcs --> [].
ws --> wsc,!,ws.
ws --> [].
wsc --> [' '].
wsc --> ['\t'].
wsc --> ['\n'].


