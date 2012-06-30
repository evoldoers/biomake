Plmake
======

This is a Makefile-like system for managing builds between multiple
dependent files. Some knowledge of prolog is assumed.

Getting Started
---------------

Install SWI-Prolog from http://www.swi-prolog.org

Get the latest plmake source from github. No installation steps are
required. Add it to your path:

    export PATH=$PATH:$HOME/plmake/bin

(changing the directory as necessary)

Get (minimal) help from the command line:

    plmake -h

Command-line
------------

  plmake [-h] [-t GNUMAKEFILE] [-l TARGET] [-n|--dry-run] [--always-make] [TARGET...]

Examples
--------

(this assumes some knowledge of make and makefiles)

plmake expects a file called `makespec.pro` to be present in your
current directory.

Assume you have two file formats, ".foo" and ".bar", and a foo2bar
converter.

Add the following rule to your makespec.pro:

    '%.bar' <-- '%.foo',
        'foo2bar $< > $@'.

Unlike makefiles, whitespace is irrelevant. Remember the closing ".",
as this is prolog syntax.

To convert a pre-existing file "x.foo" to "x.bar" type:

    plmake x.bar

Let's say we can go from a .bar to a .baz using a bar2baz
converter. We can add an additional rule:

    '%.baz' <-- '%.bar',
        'bar2baz $< > $@'.

Now if we type:

    touch x.foo
    plmake x.baz

The output will be something like:

    NT: x.baz <-- [x.bar]
      NT: x.bar <-- [x.foo]
        T: x.foo
        foo2bar x.foo > x.bar
      NT: x.bar is up to date
      bar2baz x.bar > x.baz
    NT: x.baz is up to date

In the future the output format will be more configurable. The idea is
to show the dependencies as a tree structure.

The syntax in the makespec above is designed to be similar to what is
already used in makefiles. You can bypass this and use prolog
variables. The following form is functionally equivalent:

    '$Base.bar' <-- '$Base.foo',
        'foo2bar $Base.foo > $Base.bar'.

Note that unlike Makefiles, the variables are not enclosed in
parentheses. These are not Makefile variable, but are actually prolog
variables (and must conform to prolog syntax - they must have a
leading uppercase, and only alphanumeric characters plus underscore).

You can mix and match if you like:

    '$Base.bar' <-- '$Base.foo',
        'foo2bar $< > $@'.

Unlike makefiles, plmake allows multiple variables in pattern
matching. Let's say we have a program called "align" that compares two
files producing some output (e.g. biological sequence alignment, or
ontology alignment). Assume our file convention is to suffix `.fa` on
the inputs.  We can write a makespec with the following:

    'align-$X-$Y.tbl' <-- ['$X.fa', '$Y.fa'],
        'align $X.fa $Y.fa > $@'.

(note that if we have multiple dependecies, these must be separated by
commas and enclodes in square brackets - i.e. a prolog list]

Now if we have files `x.fa` and `y.fa` we can type:

    plmake align-x-y.tbl

We can include arbitrary prolog, including both database facts and
rules. We can use these rules to control flow in a way that is more
powerful than makefiles. Let's say we only want to run a certain
program when the inputs match a certain table in our database:

    sp(mouse).
    sp(human).
    sp(zebrafish).

    'align-$X-$Y.tbl' <-- ['$X.fa', '$Y.fa'],
        {sp(X),sp(Y)},
        'align $X.fa $Y.fa > $@'.

Note that here the rule consists of 4 parts:

 * the target/output
 * dependencies
 * a prolog goal, enclosed in {}s, that is called to determine values
 * the command

In this case, the prolog goal succeeds with 9 solutions, with 3
different values for X and Y. If we type:

  plmake align-platypus-coelocanth.tbl

It will not succeed, even if the .fa files are on the filesystem. This
is because the goal cannot be satisfied for these two values.

We can create a top-level target that generates all solutions:

    % Database of species
    sp(mouse).
    sp(human).
    sp(zebrafish).

    % rule for generating a pair of (non-identical) species (asymetric)
    pair(X,Y) :- sp(X),sp(Y),X@<Y.

    % top level target
    all <-- Deps, 
      {findall( t(['align-',X,-,Y,'.tbl']),
                pair(X,Y),
                Deps)}.

    % plmake rule
    'align-$X-$Y.tbl' <-- ['$X.obo', '$Y.obo'],
        'align $X.obo $Y.obo > $@'.

It takes a little knowledge of prolog metalogical operators to
construct the generator - in future there may be a convenient
syntactic form that hides this.

Now if we type:

    plmake all

And all non-identical pairs are compared (in one direction only - the
assumption is that "align" is symmetric).

More
----

There are a few more features that will be documented in the
future. The core will likely stay minimal. The core system is
extensive and powerful so you should be able to do lots by
using/abusing either prolog or shell wrappers.

In the future there may be extensions for:

* a web-based build environment (a la Galaxy)
* semantic web enhancement (using NEPOMUK file ontology)
* using other back ends and target sources (sqlite db, REST services)
* cloud-based computing
* running computes on clusters
* make2plmake partial translator
* alternate syntaxes

Real-life examples
------------------

The makespec.pro in this project is used to make the pack release.

See http://code.google.com/p/omeo/ - in particular,
http://code.google.com/p/omeo/source/browse/trunk/build/makespec.pro

History
-------

This is a much simplified version of a system called "BioMake" from
some time ago.
