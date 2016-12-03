Biomake
=======

This is a Makefile-like system for managing builds between multiple
dependent files. No knowledge of prolog is necessary; biomake can
run off a stripped-down GNU Makefile. However, if you're prepared to
learn a little Prolog, you can do a lot more. And, really, logic
programming is the way you _should_ be specifying dependencies and
build chains; so what are you waiting for?

Getting Started
---------------

1. Install SWI-Prolog from http://www.swi-prolog.org

2. Get the latest biomake source from github. No installation steps are
required. Add it to your path (changing the directory if necessary):

    export PATH=$PATH:$HOME/biomake/bin

3. Get (minimal) help from the command line:

    biomake -h

4. Create a 'Makespec.pro' or a 'Makefile' (see below)

Alternate installation instructions
-----------------------------------

This can also be installed via the SWI-Prolog pack system

Just start SWI and type:

   ?- pack_install('biomake').

Command-line
------------

  biomake [-h] [-p MAKEPROG] [-f GNUMAKEFILE] [-l DIR] [-n|--dry-run] [-B|--always-make] [TARGETS...]

Options
-------

```
--debug TAG
    [developers] debugging messages. TAG can be build, pattern, makeprog, makefile...
-n,--dry-run 
    Print the commands that would be executed, but do not execute them
-h,--help 
    Show help
-B,--always-make 
    Always build fresh target even if dependency is up to date
-f GNUMAKEFILE
    Use a GNU Makefile as the build specification [incomplete]
-p MAKEPROG
    Use MAKEPROG as the (Prolog) build specification [default: Makespec.pro]
-l DIRECTORY
    Iterates through directory writing metadata on each file found
-H,--md5-hash 
    Use MD5 hashes instead of timestamps
-no-backtrace 
    Do not print a backtrace on error
Var=Val 
    Assign Makefile variables from command line
```

Examples
--------

(this assumes some knowledge of make and makefiles)

biomake looks for a Prolog file called `Makespec.pro` in your
current directory. If it's not there, it will try looking for a
`Makefile` in GNU Make format. The following examples describe the
Prolog syntax; GNU Make syntax is described elsewhere,
e.g. [here](https://www.gnu.org/software/make/manual/html_node/index.html).

Assume you have two file formats, ".foo" and ".bar", and a `foo2bar`
converter.

Add the following rule to your `Makespec.pro`:

    '%.bar' <-- '%.foo',
        'foo2bar $< > $@'.

Unlike makefiles, whitespace is irrelevant. However, you
do need the quotes, and remember the closing ".",
as this is prolog syntax.

If you prefer to stick with GNU Make syntax,
the above `Makespec.pro` is equivalent to the following `Makefile`:

    %.bar: %.foo
    	   foo2bar $< > $@

To convert a pre-existing file "x.foo" to "x.bar" type:

    biomake x.bar

Let's say we can go from a .bar to a .baz using a `bar2baz`
converter. We can add an additional rule:

    '%.baz' <-- '%.bar',
        'bar2baz $< > $@'.

Now if we type:

    touch x.foo
    biomake x.baz

The output shows the tree structure of the dependencies:

    Checking dependencies: test.baz <-- [test.bar]
        Checking dependencies: test.bar <-- [test.foo]
            Nothing to be done for test.foo
        Target test.bar not materialized - will rebuild if required
        foo2bar x.foo > x.bar
        test.bar is up to date
    Target test.baz not materialized - will rebuild if required
    bar2baz x.bar > x.baz
    test.baz is up to date

The syntax in the makespec above is designed to be similar to what is
already used in makefiles. You can bypass this and use prolog
variables. The following form is functionally equivalent:

    '$(Base).bar' <-- '$(Base).foo',
        'foo2bar $(Base).foo > $(Base).bar'.

The equivalent `Makefile` is

    $(Base).foo:
    	echo $(Base) >$@

    $(Base).bar: $(Base).foo
    	foo2bar $(Base).foo > $(Base).bar

If you want the variables to work as Prolog variables as well
as GNU Make variables, then they must conform to prolog syntax -
they must have a leading uppercase, and only alphanumeric characters plus underscore.

You can also use GNU Makefile constructs like automatic variables if you like:

    '$(Base).bar' <-- '$(Base).foo',
        'foo2bar $< > $@'.

Following the GNU Make convention, variable names must be enclosed in
parentheses unless they are single letters.

Unlike makefiles, biomake allows multiple variables in pattern
matching. Let's say we have a program called `align` that compares two
files producing some output (e.g. biological sequence alignment, or
ontology alignment). Assume our file convention is to suffix ".fa" on
the inputs.  We can write a `Makespec.pro` with the following:

    'align-$X-$Y.tbl' <-- ['$X.fa', '$Y.fa'],
        'align $X.fa $Y.fa > $@'.

(note that if we have multiple dependecies, these must be separated by
commas and enclodes in square brackets - i.e. a prolog list]

Now if we have files `x.fa` and `y.fa` we can type:

    biomake align-x-y.tbl

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

    biomake align-platypus-coelocanth.tbl

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

    % biomake rule
    'align-$X-$Y.tbl' <-- ['$X.obo', '$Y.obo'],
        'align $X.obo $Y.obo > $@'.

Now if we type:

    biomake all

And all non-identical pairs are compared (in one direction only - the
assumption is that the `align` program is symmetric).

More
----

Ideas for future development:

* a web-based build environment (a la Galaxy)
* semantic web enhancement (using NEPOMUK file ontology)
* using other back ends and target sources (sqlite db, REST services)
* cloud-based computing
* running computes on clusters
