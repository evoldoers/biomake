# -*- makefile-gmake -*-

prolog
mammal(mouse).
mammal(human).
sp(zebrafish).
sp(X) :- mammal(X).

pair(X,Y) :- sp(X),sp(Y),X@<Y.

make_pair(A) :-
  pair(X,Y),
  format(atom(A),"~w-~w.pair",[X,Y]).
endprolog

all: $(bagof A,make_pair(A))

$X.single:
	echo Single: $X > $@

$X-$Y.pair: $X.single $Y.single { pair(X,Y) }
	cat $X.single $Y.single > $@
	echo Files: $X.single $Y.single >> $@
