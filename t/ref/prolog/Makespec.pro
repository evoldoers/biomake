% * -*- Mode: Prolog -*- */

% Database of species
sp(mouse).
sp(human).
sp(zebrafish).

% rule for generating a pair of (non-identical) species (asymetric)
pair(X,Y) :- sp(X),sp(Y),X@<Y.

% top level target
all <-- Deps, 
{findall( t([X,-,Y,'.pair']),
          pair(X,Y),
          Deps),
format("Deps=~w~n",[Deps])}.

% biomake rules
'$X.single' <-- [],
'echo Single: $X > $@'.

'$X-$Y.pair' <-- ['$X.single', '$Y.single'],
['cat $X.single $Y.single > $@',
 'echo Files: $X.single $Y.single >> $@'].
