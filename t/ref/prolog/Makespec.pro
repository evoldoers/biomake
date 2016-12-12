% * -*- Mode: Prolog -*- */

% Database of species
mammal(mouse).
mammal(human).
sp(zebrafish).
sp(X) :- mammal(X).

% rule for generating a pair of (non-identical) species (asymmetric)
pair(X,Y) :- sp(X),sp(Y),X@<Y.

% top level target
all, {findall( t([X,-,Y,'.pair']),
               pair(X,Y),
               DEPS),
      format("DEPS=~w~n",[DEPS])}
 <-- DEPS, {true}.

% biomake rules
'$X.single' <-- [],
'echo Single: $X > $@'.

'$X-$Y.pair' <-- ['$X.single', '$Y.single'],
['cat $X.single $Y.single > $@',
 'echo Files: $X.single $Y.single >> $@'].
