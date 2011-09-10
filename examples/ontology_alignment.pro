% core ontologies to build
ont(ceph).
ont(spongebo).
ont(uberonp).

% anatomy ontologies
anatomy(uberonp).
anatomy('CL').
anatomy('ZFA').
anatomy('XAO').
anatomy('MA').
anatomy('FMA').

allow_dangling(ceph).
allow_dangling(uberon_edit).

outfmt(obo).
outfmt(owl).
outfmt(owx).
outfmt(mos).
outfmt(metadata).
outfmt(closure).

ont_fmt(Ont,Fmt) :- ont(Ont),outfmt(Fmt).

all <-- Deps,
       {findall(t([Ont,'.',Fmt]),
                ont_fmt(Ont,Fmt),
                Deps)}.

% all formats from obo
'all-%' <-- ['%.owl', '%.owx'].

% obo2owl WITH dangling
'$Base.$Fmt' <-- ['$Base.obo'],
       {allow_dangling(Base),suffix_fmt(Fmt,FmtName)},
       'obolib-obo2owl --to $FmtName --allow-dangling $< -o $@ >& $@.err'.

% obo2owl NO dangling
'$Base.$Fmt' <-- ['$Base.obo'],
       {\+ allow_dangling(Base),suffix_fmt(Fmt,FmtName)},
       'obolib-obo2owl --to $FmtName $< -o $@ >& $@.err'.


suffix_fmt(mos,manchester).
suffix_fmt(owx,owlxml).
suffix_fmt(owl,'RDFXML').

'$Base.metadata' <-- ['$Base.owl'],
       'owltools file://`pwd`/$<  --show-metadata > $@'.

'$Base.closure' <-- ['$Base.owl'],
       'owltools file://`pwd`/$<  --save-closure -c $@'.

% ----------------------------------------
% ALIGNMENT
% ----------------------------------------

anatomy_pair(A,B) :- anatomy(A),anatomy(B),A@<B.

align_all <-- Deps,
     {findall( t(['align/all-align-',A,'-',B]),
               anatomy_pair(A,B),
               Deps)}.

'align/align-$A-$B.tbl' <-- ['align/stamp'],
       {anatomy(A),anatomy(B)},
       'blip-findall -r $A -r $B -u metadata_nlp -goal index_entity_pair_label_match "entity_pair_label_reciprocal_best_intermatch(X,Y,S),class(X),class(Y)" -select "m(X,Y,S)" -use_tabs -label -no_pred > $@'.

'align/stamp' <-- [], 'touch $@'.

'align/align-$A-$B.pro' <-- ['align/align-$A-$B.tbl'],
      'cut -f1,3 $< | sort -u | tbl2p -p match > $@'.

'align/align-$A-$B-new.tbl' <-- ['align/align-$A-$B.pro'],
       'blip-findall -r $A -r $B -i uberon_edit.obo -r cell -i $< -consult align/util/align_util.pro "match(A,B),\\+generic_xref(_,A),\\+generic_xref(_,B)" -select "A-B" -use_tabs -label -no_pred > $@'.

'align/all-align-$A-$B' <-- ['align/align-$A-$B-new.tbl'].

% GENERIC

'$Dir/' <-- [],
  'mkdir -p $Dir'.




