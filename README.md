Biomake
=======

This is a [make](https://www.gnu.org/software/make/)-like utility for managing builds (or analysis workflows) between multiple
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

    `export PATH=$PATH:$HOME/biomake/bin`

3. Get (minimal) help from the command line:

    `biomake -h`

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
-h,--help 
    Show help
-v,--version 
    Show version
-n,--dry-run,--recon,--just-print 
    Print the commands that would be executed, but do not execute them
-B,--always-make 
    Always build fresh target even if dependency is up to date
-p,--prog,--makeprog MAKEPROG
    Use MAKEPROG as the (Prolog) build specification [default: Makespec.pro]
-f,--file,--makefile GNUMAKEFILE
    Use a GNU Makefile as the build specification
-m,--eval,--makefile-syntax STRING
    Evaluate STRING as GNU Makefile syntax
-P,--eval-prolog,--makespec-syntax STRING
    Evaluate STRING as Prolog Makespec syntax
-I,--include-dir DIR
    Specify search directory for included Makefiles
--target TARGET
    Force biomake to recognize a target even if it looks like an option
-T FILE
    Translate GNU Makefile to Prolog Makeprog syntax
-W,--what-if,--new-file,--assume-new TARGET
    Pretend that TARGET has been modified
-o,--old-file,--assume-old TARGET
    Do not remake TARGET, or remake anything on account of it
-k,--keep-going 
    Keep going after error
-S,--no-keep-going,--stop 
    Stop after error
-t,--touch 
    Touch files (and update MD5 hashes, if appropriate) instead of running recipes
Var=Val 
    Assign Makefile variables from command line
-l DIRECTORY
    Iterates through directory writing metadata on each file found
-s,--quiet,--silent 
    Silent operation; do not print recipes as they are executed
--one-shell 
    Run recipes in single shell (equivalent to GNU make's .ONESHELL)
-H,--md5-hash 
    Use MD5 hashes instead of timestamps
-Q,--queue-engine ENGINE
    Queue recipes using ENGINE (supported: test,sge,pbs,slurm,poolq)
-j,--jobs JOBS
    Number of job threads (poolq engine)
--qsub-exec PATH
    Path to qsub (sge,pbs) or sbatch (slurm)
--qdel-exec PATH
    Path to qdel (sge,pbs) or scancel (slurm)
--queue-args "ARGS"
    Queue-specifying arguments for qsub/qdel (sge,pbs) or sbatch/scancel (slurm)
--qsub-args "ARGS"
    Additional arguments for qsub (sge,pbs) or sbatch (slurm)
--qdel-args "ARGS"
    Additional arguments for qdel (sge,pbs) or scancel (slurm)
--flush,--qsub-flush <target or directory>
    Erase all jobs for given target/dir
--debug MSG
    [developers] Debugging messages. MSG can be build, pattern, makefile, md5...
--trace predicate
    [developers] Print debugging trace for given predicate
--no-backtrace 
    [developers] Do not print a backtrace on error
```

Examples
--------

(this assumes some knowledge of GNU Make and [Makefiles](https://www.gnu.org/software/make/manual/html_node/index.html))

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

The equivalent `Makefile` would be this...

    $(Base).foo:
    	echo $(Base) >$@

    $(Base).bar: $(Base).foo
    	foo2bar $(Base).foo > $(Base).bar

...although this isn't _strictly_ equivalent, since unbound variables
don't work the same way in GNU Make as they do in Biomake
(Biomake will try to use them as wildcards for [pattern-matching](#PatternMatching),
whereas GNU Make will just replace them with the empty string - which is also the default behavior
for Biomake if they occur outside of a pattern-matching context).

If you want variables to work as Prolog variables as well
as GNU Make variables, then they must conform to Prolog syntax:
they must have a leading uppercase, and only alphanumeric characters plus underscore.

You can also use GNU Makefile constructs, like automatic variables (`$<`, `$@`, `$*`, etc.), if you like:

    '$(Base).bar' <-- '$(Base).foo',
        'foo2bar $< > $@'.

Following the GNU Make convention, variable names must be enclosed in
parentheses unless they are single letters.

<a name="PatternMatching"></a>
Pattern-matching
----------------

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
