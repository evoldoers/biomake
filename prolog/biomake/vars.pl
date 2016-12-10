% * -*- Mode: Prolog -*- */

:- module(vars,
          [
	   eval_bagof/3
           ]).

/** 

  A minimally-cluttered namespace for user variable bindings in Makeprogs & Makefiles.

  */

:- use_module(library(biomake/biomake)).

% We have to redefine these operators, ugh
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
