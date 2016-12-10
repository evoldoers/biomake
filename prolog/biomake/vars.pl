% * -*- Mode: Prolog -*- */

:- module(vars,
          [
	   read_makeprog_stream/4,

	   eval_string_as_makeprog_term/3,
	   eval_atom_as_makeprog_term/3,

	   eval_bagof/3
           ]).

:- use_module(library(biomake/biomake)).

% we have to duplicate these op declarations, ugh
:- user:op(1100,xfy,<--).
:- user:op(1101,xfy,?=).
:- user:op(1102,xfy,:=).
:- user:op(1103,xfy,+=).
:- user:op(1104,xfy,=*).

% although it is called from another module, eval_bagof has to be in this module
% because term assertions from the makeprog go in this namespace.
eval_bagof(TemplateStr,GoalStr,Result) :-
    format(atom(BagofAtom),"bagof(~w,~w,List)",[TemplateStr,GoalStr]),
    atom_to_term(BagofAtom,BagofTerm,Bindings),
    call_without_backtrace(BagofTerm),
    member(('List' = List), Bindings),
    atomic_list_concat(List," ",Result),
    !.
eval_bagof(_,_,"").

read_makeprog_stream(IO,Opts,Opts,[]) :-
        at_end_of_stream(IO),
	!,
	close(IO).

read_makeprog_stream(IO,OptsOut,OptsIn,[Term|Terms]) :-
        read_term(IO,Term,[variable_names(VNs),
                           syntax_errors(error),
                           module(vars)]),
        debug(makeprog,'adding: ~w (variables: ~w)',[Term,VNs]),
        add_spec_clause(Term,VNs,Opts,OptsIn),
	read_makeprog_stream(IO,OptsOut,Opts,Terms).

eval_string_as_makeprog_term(String,OptsOut,OptsIn) :-
        atom_string(Atom,String),
        eval_atom_as_makeprog_term(Atom,OptsOut,OptsIn).

eval_atom_as_makeprog_term(Atom,OptsOut,OptsIn) :-
        read_term_from_atom(Atom,Term,[variable_names(VNs),
				       syntax_errors(error),
				       module(vars)]),
        debug(makeprog,'adding: ~w (variables: ~w)',[Term,VNs]),
        add_spec_clause(Term,VNs,OptsOut,OptsIn).
