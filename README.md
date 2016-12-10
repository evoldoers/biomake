Biomake
=======

This is a [make](https://www.gnu.org/software/make/)-like utility for managing builds (or analysis workflows) involving multiple
dependent files.
It supports most of the functionality of GNU Make, along with neat extensions like
cluster-based job processing, multiple wildcards per target, MD5 checksums instead of timestamps,
and declarative logic programming in Prolog.

Indeed: [Prolog](https://en.wikipedia.org/wiki/Prolog).
No knowledge of the dark logical arts is necessary to use Biomake; the software can be
run directly off a GNU Makefile. However, if you know (or are prepared to
learn) a little Prolog, you can do a lot more. And, really, logic
programming is the way you _should_ be specifying workflow dependencies and
build chains; so what are you waiting for?

Getting Started
---------------

1. Install SWI-Prolog from http://www.swi-prolog.org

2. Get the latest biomake source from github. No installation steps are
required. Just add it to your path (changing the directory if necessary):

    `export PATH=$PATH:$HOME/biomake/bin`
Alternatively, if you want to install it in `/usr/local/bin`, type `make install`.
(This just creates a symlink, so don't remove the `biomake` directory after installation.)
You can also try `make test` to run the test suite.

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
-P,--eval-prolog,--makeprog-syntax STRING
    Evaluate STRING as Prolog Makeprog syntax
-I,--include-dir DIR
    Specify search directory for included Makefiles
--target TARGET
    Force biomake to recognize a target even if it looks like an option
-T,--translate,--save-prolog FILE
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
    Run recipes in single shell (equivalent to GNU Make's .ONESHELL)
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

biomake looks for a Prolog file called `Makespec.pro` (or `Makeprog`) in your
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
as this is Prolog syntax.

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

The syntax in the makeprog above is designed to be similar to what is
already used in makefiles. You can bypass this and use Prolog
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
commas and enclodes in square brackets - i.e. a Prolog list)

Now if we have files `x.fa` and `y.fa` we can type:

    biomake align-x-y.tbl

We could achieve the same thing with the following GNU `Makefile`:

    align-$X-$Y.tbl: $X.fa $Y.fa
        align $X.fa $Y.fa > $@

This is already an improvement over GNU Make, which only allows a single wildcard.
However, the Prolog version allows us to do even fancier things with logic.
Specifically, we can add arbitrary Prolog, including both database facts and
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
 * a Prolog goal, enclosed in `{}`s, that is called to determine values
 * the command

In this case, the Prolog goal succeeds with 9 solutions, with 3
different values for X and Y. If we type:

    biomake align-platypus-coelocanth.tbl

It will not succeed, even if the .fa files are on the filesystem. This
is because the goal cannot be satisfied for these two values.

We can create a top-level target that generates all solutions:

    % Database of species
    sp(mouse).
    sp(human).
    sp(zebrafish).

    % rule for generating a pair of (non-identical) species (asymmetric)
    pair(X,Y) :- sp(X),sp(Y),X@<Y.

    % top level target
    all <-- Deps, 
      {findall( t(['align-',X,-,Y,'.tbl']),
                pair(X,Y),
                Deps )}.

    % biomake rule
    'align-$X-$Y.tbl' <-- ['$X.obo', '$Y.obo'],
        'align $X.obo $Y.obo > $@'.

Now if we type:

    biomake all

And all non-identical pairs are compared (in one direction only - the
assumption is that the `align` program is symmetric).

Translation to Prolog
---------------------

You can parse a GNU Makefile and save the corresponding Prolog version using the `-T` option
(long-form `--translate`).

Make-like features
------------------

Biomake supports most of the functionality of GNU Make, including
- different [flavors of variable](https://www.gnu.org/software/make/manual/make.html#Flavors) (recursive, expanded, etc.)
- various ways of [setting variables](https://www.gnu.org/software/make/manual/html_node/Setting.html)
- [appending to variables](https://www.gnu.org/software/make/manual/html_node/Appending.html)
- [multi-line variables](https://www.gnu.org/software/make/manual/html_node/Multi_002dLine.html)
- [automatic variables](https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html) such as `$<`, `$@`, `$^`, `$(@F)`, etc.
- [substitution references](https://www.gnu.org/software/make/manual/html_node/Substitution-Refs.html)
- [computed variable names](https://www.gnu.org/software/make/manual/html_node/Computed-Names.html)
- [all the text functions](https://www.gnu.org/software/make/manual/html_node/Text-Functions.html)
- [all the filename functions](https://www.gnu.org/software/make/manual/html_node/File-Name-Functions.html)
- [the shell function](https://www.gnu.org/software/make/manual/html_node/Shell-Function.html)
- [user-defined functions](https://www.gnu.org/software/make/manual/html_node/Call-Function.html)
- [errors and warnings](https://www.gnu.org/software/make/manual/html_node/Make-Control-Functions.html)
- many of the same [command-line options](https://www.gnu.org/software/make/manual/html_node/Options-Summary.html)
- [conditional syntax](https://www.gnu.org/software/make/manual/html_node/Conditionals.html) and [conditional functions](https://www.gnu.org/software/make/manual/html_node/Conditional-Functions.html)
- the [include](https://www.gnu.org/software/make/manual/html_node/Include.html) directive
- various other quirks of GNU Make syntax e.g. single-line recipes, forced rebuilds

Differences from GNU Make
-------------------------

There are slight differences in the way variables are expanded, which arise from the fact that Biomake
treats variable expansion as a post-processing step (part of the language) rather than a pre-processing step (which is how GNU Make does it).
In Biomake, variable expansions must be aligned with the overall syntactic structure; they cannot span multiple syntactic elements.

As a concrete example, GNU Make allows this sort of thing:

~~~~
RULE = target: dep1 dep2
$(RULE) dep3
~~~~

which (in GNU Make, but not biomake) expands to

~~~~
target: dep1 dep2 dep3
~~~~

That is, the expansion of the `RULE` variable spans both the target list and the start of the dependency list.
To emulate this behavior faithfully, Biomake would have to do the variable expansion in a separate preprocessing pass - which would mean we couldn't translate variables directly into Prolog.
We think it's worth sacrificing this edge case in order to maintain the semantic parallel between Makefile variables and Prolog variables, which allows for some powerful constructs.

The implementation of [conditional syntax](https://www.gnu.org/software/make/manual/html_node/Conditionals.html)
(`ifeq`, `ifdef` and the like) must also be aligned with the syntax: you can only place a conditional
at a point where a variable assignment, recipe, or `include` directive could go
(i.e. at the top level of the `Makefile` grammar).

Unlike GNU Make, Biomake does not offer domain-specific language extensions in [Scheme](https://www.gnu.org/software/guile/)
(even though this is one of the cooler aspects of GNU Make), but you can program it in Prolog instead - it's quite hackable.

Arithmetic functions
--------------------

Biomake provides a few extra functions for arithmetic on lists:

- `$(iota N)` returns a space-separated list of numbers from `1` to `N`
- `$(iota S,E)` returns a space-separated list of numbers from `S` to `E`
- `$(add X,L)` adds `X` to every element of the space-separated list `L`
- `$(multiply Y,L)` multiplies every element of the space-separated list `L` by `Y`
- `$(divide Z,L)` divides every element of the space-separated list `L` by `Z`

MD5 hashes
----------

Instead of using file timestamps, which are fragile (especially on networked filesystems),
Biomake can optionally use MD5 checksums to decide when to rebuild files.
Turn on this behavior with the `-H` options (long form `--md5-hash`).

Biomake uses the external program `md5` to do checksums (available on OS X), or `md5sum` (available on Linux).
If neither of these are found, Biomake falls back to using the SWI-Prolog md5 implementation;
this does however require loading the entire file into memory (which may be prohibitive for large files).

Queues
------

To run jobs in parallel, locally or on a cluster, you need to specify a queueing engine
using the `-Q` option (long form `--queue-engine`). Note that, unlike with GNU Make, multi-threading is not triggered
simply by specifying the number of threads with `-j`; you need `-Q` as well.

There are several queueing engines currently supported:

- `-Q poolq` uses an internal thread pool for running jobs in parallel on the same machine that `biomake` is running on
- `-Q sge` uses [Sun Grid Engine](https://en.wikipedia.org/wiki/Oracle_Grid_Engine)
- `-Q pbs` uses [PBS](https://en.wikipedia.org/wiki/Portable_Batch_System)
- `-Q slurm` uses [SLURM](https://slurm.schedmd.com/)

For Sun Grid Engine, PBS and SLURM, the paths to the relevant job control executables, and any arguments to those executables
(such as the name of the queue that jobs should be run on), can be controlled using various command-line arguments.

More
----

Ideas for future development:

* a web-based build environment (a la Galaxy)
* semantic web enhancement (using NEPOMUK file ontology)
* using other back ends and target sources (sqlite db, REST services)
* cloud-based computing
