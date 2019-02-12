[![Build Status](https://travis-ci.org/evoldoers/biomake.svg?branch=master)](https://travis-ci.org/evoldoers/biomake)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

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
learn) a little Prolog, you can do a lot more.
Makefiles are logic programs: their power comes from combining a _declarative_ specification of dependencies
with _procedural_ shell scripts to build targets.
Prolog is a simple but expressive language for logic programming
that allows Makefile rules to be extended in sophisticated and flexible ways.

Getting Started
---------------

1. Install SWI-Prolog from http://www.swi-prolog.org

2. Get the latest biomake source from github. No installation steps are
required. Just add it to your path (changing the directory if necessary):

    `export PATH=$PATH:$HOME/biomake/bin`

3. Get (minimal) help from the command line:

    `biomake -h`

4. Create a 'Makefile' or a 'Makeprog' (see below)

Alternate installation instructions
-----------------------------------

If you want to install biomake system-wide, instead of adding it to your path, type `make install` (or `bin/biomake install`) in the top level directory of the repository.
This will copy the repository into `/usr/local/share` and create a symlink to `/usr/local/bin`.
(If you just want to create the symlink and leave the repository where it is, type `make symlink` instead.)

You can also try `make test` (or, equivalently, `biomake test`) to run the test suite.

The program can also be installed via the SWI-Prolog pack system.
Just start SWI and type:

    ?- pack_install('biomake').

Command-line
------------

    biomake [OPTIONS] [TARGETS]

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
-f,--file,--makefile GNUMAKEFILE
    Use a GNU Makefile as the build specification [default: Makefile]
-p,--prog,--makeprog MAKEPROG
    Use MAKEPROG as the (Prolog) build specification [default: Makeprog]
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
-N,--no-dependencies 
    Do not test or rebuild dependencies
-D,--define Var Val
    Assign Makefile variables from command line
Var=Val 
    Alternative syntax for '-D Var Val'
-l DIRECTORY
    Iterates through directory writing metadata on each file found
-s,--quiet,--silent 
    Silent operation; do not print recipes as they are executed
--one-shell 
    Run recipes in single shell (loosely equivalent to GNU Make's .ONESHELL)
-y,--sync URI
    Synchronize current working directory to a remote URI. If no --sync-exec is specified, S3-form URIs (s3://mybucket/my/path) are handled using the AWS CLI tool; other URIs will be passed to rsync.
--sync-exec COMMAND
    Specify executable for --sync.
-H,--md5-hash 
    Use MD5 hashes instead of timestamps
-C,--no-md5-cache 
    Recompute MD5 checksums whenever biomake is restarted
-M,--no-md5-timestamp 
    Do not recompute MD5 checksums when timestamps appear stale
-Q,--queue-engine ENGINE
    Queue recipes using ENGINE (supported: poolq,sge,pbs,slurm,test)
-j,--jobs JOBS
    Number of job threads (poolq engine)
--qsub-exec PATH
    Path to qsub (sge,pbs) or sbatch (slurm)
--qdel-exec PATH
    Path to qdel (sge,pbs) or scancel (slurm)
--queue-args 'ARGS'
    Queue-specifying arguments for qsub/qdel (sge,pbs) or sbatch/scancel (slurm)
--qsub-args,--sbatch-args 'ARGS'
    Additional arguments for qsub (sge,pbs) or sbatch (slurm)
--qsub-use-biomake,--sbatch-use-biomake 
    Force qsub/sbatch to always call biomake recursively
--qsub-biomake-args,--sbatch-biomake-args 'ARGS'
    Arguments passed recursively to biomake by qsub/sbatch (default: '-N')
--qsub-header,--sbatch-header 'HEADER'
    Header for qsub (sge,pbs) or sbatch (slurm)
--qsub-header-file,--sbatch-header-file 'FILENAME'
    Header file for qsub (sge,pbs) or sbatch (slurm)
--qdel-args,--scancel-args 'ARGS'
    Additional arguments for qdel (sge,pbs) or scancel (slurm)
--flush,--qsub-flush <target or directory>
    Erase all jobs for given target/dir
-d 
    [developers] Print debugging messages. Equivalent to '--debug verbose'
--debug MSG
    [developers] Richer debugging messages. MSG can be verbose, bindrule, build, pattern, makefile, makeprog, md5...
--trace PREDICATE
    [developers] Print debugging trace for given predicate
--no-backtrace 
    [developers] Do not print a backtrace on error
```

Embedding Prolog in Makefiles
-----------------------------

Brief overview:

- Prolog can be embedded within `prolog` and `endprolog` directives
- `$(bagof Template,Goal)` expands to the space-separated `List` from the Prolog `bagof(Template,Goal,List)`
- Following the target list with `{target_goal}` causes the rule to match only if `target_goal` is satisfied. The target goal will be tested _before_ any dependencies are built. The special variable `TARGET`, if used, will be bound to the target filename (i.e. `$@`)
- Following the dependency list with `{deps_goal}` causes the recipe to be executed only if `deps_goal` is satisfied. The deps goal will be tested _after_ any dependencies are built (so it can examine the dependency files). The special variables `TARGET` and `DEPS`, if used, will be bound to the target and dependency-list (i.e. `$@` and `$^`, loosely speaking; except the latter is a true Prolog list, not encoded as a string with whitespace separators as in GNU Make)

Examples
--------

This assumes some knowledge of GNU Make and [Makefiles](https://www.gnu.org/software/make/manual/html_node/index.html).

Unlike makefiles, biomake allows multiple variables in pattern
matching. Let's say we have a program called `align` that compares two
files producing some output (e.g. biological sequence alignment, or
ontology alignment). Assume our file convention is to suffix ".fa" on
the inputs.  We can write a `Makefile` with the following:

    align-$X-$Y: $X.fa $Y.fa
        align $X.fa $Y.fa > $@

Now if we have files `x.fa` and `y.fa` we can type:

    biomake align-x-y

Prolog extensions allow us to do even fancier things with logic.
Specifically, we can embed arbitrary Prolog, including both database facts and
rules. We can use these rules to control flow in a way that is more
powerful than makefiles.

Let's say we only want to run a certain program when the inputs match a certain table in our database.
We can embed Prolog in our Makefile as follows:

    prolog
    sp(mouse).
    sp(human).
    sp(zebrafish).
    endprolog

    align-$X-$Y: $X.fa $Y.fa {sp(X),sp(Y)}
        align $X.fa $Y.fa > $@

The lines beginning `sp` between `prolog` and `endprolog` define the set of species that we want the rule to apply to.
The rule itself consists of 4 parts:

 * the target (`align-$X-$Y`)
 * the dependencies (`$X.fa` and `$Y.fa`)
 * a Prolog goal, enclosed in braces (`{sp(X),sp(Y)}`), that is used as an additional logic test of whether the rule can be applied
 * the command (`align ...`)

In this case, the Prolog goal succeeds with 9 solutions, with 3
different values for `X` and `Y`. If we type...

    biomake align-platypus-coelacanth

...it will not succeed, even if the .fa files are on the filesystem. This
is because the goal `{sp(X),sp(Y)}` cannot be satisfied for these two values of `X` and `Y`.

To get a list of all matching targets,
we can use the special BioMake function `$(bagof...)`
which wraps the Prolog predicate [bagof/3](http://www.swi-prolog.org/pldoc/man?predicate=bagof/3).
The following example also uses the Prolog predicates
[format/2](http://www.swi-prolog.org/pldoc/man?predicate=format/2)
and
[format/3](http://www.swi-prolog.org/pldoc/man?predicate=format/3),
for formatted output:

~~~~
prolog

sp(mouse).
sp(human).
sp(zebrafish).

ordered_pair(X,Y) :- sp(X),sp(Y),X@<Y.

make_filename(F) :-
  ordered_pair(X,Y),
  format(atom(F),"align-~w-~w",[X,Y]).

endprolog

all: $(bagof F,make_filename(F))

align-$X-$Y: $X.fa $Y.fa { ordered_pair(X,Y),
                           format("Matched ~w <-- ~n",[TARGET,DEPS]) },
    align $X.fa $Y.fa > $@
~~~~

Now if we type...

    biomake all

...then all non-identical ordered pairs will be compared
(since we have required them to be _ordered_ pairs, we get e.g. "mouse-zebrafish" but not "zebrafish-mouse";
the motivation here is that the `align` program is symmetric, and so only needs to be run once per pair).

In these examples, the goals between braces are tested _after_ the dependencies.
This means that any Prolog code in these braces can safely examine the dependency files
(for example, you could constrain a rule to apply only if a dependency file was below a certain size,
or in a certain file format).
You can also place a Prolog goal (in braces) between the target list and the colon;
it will then be tested after the target name has been matched,
but _before_ trying to build any dependencies.
In such a goal, you can use the `TARGET` variable but not the `DEPS` variable.

Programming directly in Prolog
------------------------------

If you are a Prolog wizard who finds embedding Prolog in Makefiles too cumbersome, you can use a native Prolog-like syntax.
Biomake looks for a Prolog file called `Makeprog` (or `Makespec.pro`) in your
current directory. (If it's not there, it will try looking for a
`Makefile` in GNU Make format. The following examples describe the
Prolog syntax.)

Assume you have two file formats, ".foo" and ".bar", and a `foo2bar`
converter.

Add the following rule to your `Makeprog`:

    '%.bar' <-- '%.foo',
        'foo2bar $< > $@'.

Unlike makefiles, whitespace is irrelevant. However, you
do need the quotes, and remember the closing ".",
as this is Prolog syntax.

If you prefer to stick with GNU Make syntax,
the above `Makeprog` is equivalent to the following `Makefile`:

    %.bar: %.foo
    	   foo2bar $< > $@

To convert a pre-existing file "x.foo" to "x.bar" type:

    biomake x.bar

Let's say we can go from a .bar to a .baz using a `bar2baz`
converter. We can add an additional rule:

    '%.baz' <-- '%.bar',
        'bar2baz $< > $@'.

Now if we type...

    touch x.foo
    biomake x.baz

...we get something like the following output:

~~~~
% Checking dependencies: x.baz <-- [x.bar]
%  Checking dependencies: x.bar <-- [x.foo]
%   Nothing to be done for x.foo
%  Target x.bar not materialized - build required
 foo2bar x.foo > x.bar
%  x.bar built
% Target x.baz not materialized - build required
bar2baz x.bar > x.baz
% x.baz built
~~~~

The syntax in the makeprog above is designed to be similar to the automatic variable syntax
already used in makefiles. You can bypass this and use Prolog
variables. The following form is functionally equivalent:

    '$(Base).bar' <-- '$(Base).foo',
        'foo2bar $(Base).foo > $(Base).bar'.

The equivalent `Makefile` would be this...

    $(Base).bar: $(Base).foo
    	foo2bar $(Base).foo > $(Base).bar

...although strictly speaking, this is only equivalent if you are using Biomake;
GNU Make's treatment of this Makefile isn't quite equivalent, since unbound variables
don't work the same way in GNU Make as they do in Biomake
(Biomake will try to use them as wildcards for pattern-matching,
whereas GNU Make will just replace them with the empty string - which is also the default behavior
for Biomake if they occur outside of a pattern-matching context).

Following the GNU Make convention, variable names must be enclosed in
parentheses unless they are single letters.

Automatic translation to Prolog
-------------------------------

You can parse a GNU Makefile (including Biomake-specific extensions, if any)
and save the corresponding Prolog syntax using the `-T` option
(long-form `--translate`).

Here is the translation of the Makefile from the previous section (lightly formatted for clarity):

~~~
sp(mouse).
sp(human).
sp(zebrafish).

ordered_pair(X,Y):-
 sp(X),
 sp(Y),
 X@<Y.

make_filename(F):-
 ordered_pair(X,Y),
 format(atom(F),"align-~w-~w",[X,Y]).

"all" <-- "$(bagof F,make_filename(F))".

"align-$X-$Y" <--
 ["$X.fa","$Y.fa"],
 {ordered_pair(X,Y),
  format("Matched ~w <-- ~n",[TARGET,DEPS])},
 "align $X.fa $Y.fa > $@".
~~~

Note how the list of dependencies in the second rule, which contains more than one dependency (`$X.fa` and `$Y.fa`), is enclosed in square brackets, i.e. a Prolog list (`["$X.fa","$Y.fa"]`).
The same syntax applies to rules which have lists of multiple targets, or multiple executables.

The rule for target `all` in this translation involves a call to the Biomake function `$(bagof ...)`,
but (as noted) this function is just a wrapper for the Prolog `bagof/3` predicate.
The automatic translation is not smart enough to remove this layer of wrapping,
but we can do so manually, yielding a clearer program:

~~~
sp(mouse).
sp(human).
sp(zebrafish).

ordered_pair(X,Y):-
 sp(X),
 sp(Y),
 X@<Y.

make_filename(F):-
 ordered_pair(X,Y),
 format(atom(F),"align-~w-~w",[X,Y]).

"all", {bagof(F,make_filename(F),DepList)} <-- DepList, {true}.

"align-$X-$Y" <--
 ["$X.fa","$Y.fa"],
 {ordered_pair(X,Y),
  format("Matched ~w <-- ~n",[TARGET,DEPS])},
 "align $X.fa $Y.fa > $@".
~~~

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
- [wildcards in dependency lists](https://www.gnu.org/software/make/manual/html_node/Wildcards.html)
- [phony targets](https://www.gnu.org/software/make/manual/html_node/Phony-Targets.html)
- various other quirks of GNU Make syntax e.g. single-line recipes, forced rebuilds

Currently unsupported features of GNU Make
------------------------------------------

The following features of GNU Make are not (yet) implemented:

- [Order-only prerequisites](https://www.gnu.org/software/make/manual/html_node/Prerequisite-Types.html)
- [Directory search](https://www.gnu.org/software/make/manual/html_node/Directory-Search.html)
- Many of the [special built-in targets](https://www.gnu.org/software/make/manual/html_node/Special-Targets.html), with some exceptions:
    - `.PHONY` is implemented
    - `.SILENT` is implemented
    - `.NOTPARALLEL` is implemented
    - `.ONESHELL` is implemented
    - `.IGNORE` is implemented
    - `.DELETE_ON_ERROR` is implemented
    - `.SECONDARY` is implicit and `.INTERMEDIATE` is unsupported: Biomake never removes intermediate files (unless `.DELETE_ON_ERROR` is specified)
    - `.PRECIOUS` is implicit for all targets
    - `.SECONDEXPANSION` is implicit
    - `.SUFFIXES` is unsupported (or implicit with no dependencies), since suffix rules are unsupported
    - other special targets not mentioned in the above list are not supported (they'll just be parsed as regular targets, i.e. ignored)
- [Multiple rules per target](https://www.gnu.org/software/make/manual/html_node/Multiple-Rules.html)
- [Static pattern rules](https://www.gnu.org/software/make/manual/html_node/Static-Pattern.html)
- [Double-colon rules](https://www.gnu.org/software/make/manual/html_node/Double_002dColon.html)
- [Suffix rules](https://www.gnu.org/software/make/manual/html_node/Suffix-Rules.html)
- Modifiers in recipe lines are only partially supported:
    - The [+ sign to force execution during dry runs](https://www.gnu.org/software/make/manual/html_node/Instead-of-Execution.html) is _not_ supported
    - The [- sign to suppress errors in recipes](https://www.gnu.org/software/make/manual/html_node/Errors.html) _is_ supported
    - The [@ sign to execute recipe lines silently](https://www.gnu.org/software/make/manual/html_node/Echoing.html) _is_ supported
- The [export](https://www.gnu.org/software/make/manual/html_node/Variables_002fRecursion.html) keyword is supported, but "unexport" and "override" are _not_ supported

Please [submit a GitHub issue](https://github.com/evoldoers/biomake/issues) if any of these are important to you.

Other differences from GNU Make
-------------------------------

There are slight differences in the way variables are expanded, which arise from the fact that Biomake
treats variable expansion as a post-processing step (performed at the last possible moment) rather than a pre-processing step (which is how GNU Make does it - at least partly).

Specifically, Biomake parses the Makefile, reading all variable and recipe declarations into memory, and only when the build begins are variables expanded.
The only exception to this is when variables are used in [conditional syntax](https://www.gnu.org/software/make/manual/html_node/Conditionals.html),
to control which parts of the Makefile are actually read:
these variables are expanded at parse-time.

In contrast, GNU Make expands variables in dependency lists at parse time (along with conditional syntax),
but expands variables in recipe bodies later.

This can cause differences between GNU and Biomake in situations where variables change value throughout the Makefile.
These situations are usually counter-intuitive anyway, as the following example illustrates.
This Makefile, which might naively be expected to print `hello everybody`,
in fact prints `hello world` when run with `make test`, but `goodbye world` when run with `biomake test`:

~~~~
A = hello
B = everybody

test: $A
	@echo $B

A = goodbye
B = world

hello goodbye:
	@echo $@
~~~~

This example gets even more counterintuitive if we wrap the `test` recipe with conditional syntax.
It still gives the same results, though: `hello world` when run with `make test`, and `goodbye world` when run with `biomake test`.

~~~~
A = hello
B = everybody

ifeq ($B,everybody)
test: $A
	@echo $B
else
test:
	@echo Curioser and curioser
endif

A = goodbye
B = world

hello goodbye:
	@echo $@
~~~~

Another consequence is that, when using Biomake, variable expansions must be aligned with the overall syntactic structure; they cannot span multiple syntactic elements.
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
(`ifeq`, `ifdef` and the like) similarly requires that syntax to be aligned with the overall structure:
you can only place a conditional at a point where a variable assignment, recipe, or `include` directive could go
(i.e. at the top level of the `Makefile` grammar).
Conditional syntax _is_ implemented as a preprocessing step.

Unlike GNU Make, Biomake does not offer domain-specific language extensions in [Scheme](https://www.gnu.org/software/guile/)
(even though this is one of the cooler aspects of GNU Make), but you can program it in Prolog instead - it's quite hackable.

Detailed build logic
--------------------

The build logic for biomake should _usually_ yield the same results as GNU Make, though there may be subtle differences.
The GNU Make [algorithm](https://www.gnu.org/software/make/manual/html_node/Implicit-Rule-Search.html) differs in the details.

Before attempting to build a target `T` using a rule `R`, Biomake performs the following steps:
- It tries to match the target name `T` to one of the target names in `R`
- It tests whether the Prolog _target goal_ (if there is one) is satisfied
- It checks whether there is a _theoretical path_ to all the dependencies. A theoretical path to a dependency `D` exists if either of the following is true:
    - There is a rule that could be used to build `D`, the target goal for that rule is satisfied, and there is a theoretical path to all the dependencies of that rule;
    - File `D` already exists, and the only applicable rules to rebuild `D`, if any exist at all, are wildcard (pattern) rules; that is, there are no rules that _explicitly and uniquely_ rebuild `D`.
- It attempts to build all the dependencies
- It tests whether the Prolog _deps goal_ (if there is one) is satisfied
- It tests whether the target is stale. Details depend on the various options:
    - Command-line options for marking targets as stale or new (`-W`, `-B`, `-o`) can override any of the following behavior
    - If using the queueing engine, or if doing a dry-run (`-n`), targets are flagged as stale if any of their dependency tree has been rebuilt (or submitted to the queue for a rebuild);
    - If using MD5 signatures (and _not_ the queueing engine), a target is stale if its MD5 checksum appears to be out of date;
    - If using MD5 _and_ queues, the MD5 signature will not be checked until the queueing engine executes the job (which is guaranteed to happen after any dependencies are rebuilt). Otherwise the dependencies might change after the MD5 checksum was tested. This is accomplished by wrapping the recipe script with a recursive call to biomake; so biomake has to be available on the worker machines, and not just the cluster head. (The same is true, incidentally, when using a cluster to execute any rule that has a Prolog deps goal: the submitted job is wrapped by biomake, in order that the goal can be tested after the dependencies are built.)
    - Otherwise (no queues and no MD5), Biomake looks at the file timestamps and/or the dependency tree.

If any of these tests fail, Biomake will backtrack and attempt to build the target using a different rule, or a different pattern-match to the same rule.
If all the tests pass, Biomake will commit to using the rule, and will attempt to execute the recipe using the shell (or the queueing engine).

Note that the target goal is tested multiple times (to plan theoretical build paths) and so should probably not have side effects.
The deps goal is tested later, and only once for every time the rule is bound, so it is a bit safer for the deps goal to have side effects.

Failure during execution of the recipe (or execution of any recipes in the dependency tree) will never cause Biomake to backtrack; it will either halt, or (if the `-k` command-line option was specified) soldier on obliviously.

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
Turn on this behavior with the `-H` option (long form `--md5-hash`).

Biomake uses the external program `md5` to do checksums (available on OS X), or `md5sum` (available on Linux).
If neither of these are found, Biomake falls back to using the SWI-Prolog md5 implementation;
this does however require loading the entire file into memory (which may be prohibitive for large files).

Queues
------

To run jobs in parallel, locally or on a cluster, you need to specify a queueing engine
using the `-Q` option (long form `--queue-engine`). Note that, unlike with GNU Make, multi-threading is not activated
simply by specifying the number of threads with `-j`; you need `-Q` as well.

There are several queueing engines currently supported:

- `-Q poolq` uses an internal thread pool for running jobs in parallel on the same machine that `biomake` is running on
- `-Q sge` uses [Sun Grid Engine](https://en.wikipedia.org/wiki/Oracle_Grid_Engine) or compatible (e.g. [Open Grid Scheduler](http://gridscheduler.sourceforge.net/))
- `-Q pbs` uses [PBS](https://en.wikipedia.org/wiki/Portable_Batch_System)
- `-Q slurm` uses [Slurm](https://slurm.schedmd.com/)
- `-Q test` just runs the jobs synchronously. Used for testing purposes only

For Sun Grid Engine, PBS and Slurm, the paths to the relevant job control executables, and any arguments to those executables
(such as the name of the queue that jobs should be run on), can be controlled using various command-line arguments.
In particular, the `--qsub-args` command-line option (applying to all recipes)
and the `QsubArgs` Prolog variable (on a per-recipe basis, in the target goal)
can be used to pass parameters such as the queue name.

Here's an example of using `QsubArgs`:

~~~~
my_target { QsubArgs = '--cores-per-socket=4' } : my_dependency
    do_something >$@
~~~~

Note that `QsubArgs` has to be set in the target goal, not the deps goal
(since the job is submitted to the queueing engine before the dependencies are guaranteed to have been built).

Similarly, you can use the `QsubHeader` variable (or the `--qsub-header` command-line option) to add header lines to the wrapper script that is submitted to the queue engine
(for example, to provide queue configuration directives),
or you can use `QsubHeaderFile` (or `--qsub-header-file`) to specify the filename of a header file to include.

The names of these Prolog variables for fine-grained queue configuration (`QsubArgs`, `QsubHeader`, `QsubHeaderFile`) are the same for Slurm as for SGE and PBS,
even though the batch submission command for Slurm is `sbatch` and not `qsub`.

More
----

Ideas for future development:

* a web-based build environment (a la Galaxy)
* semantic web enhancement (using NEPOMUK file ontology)
* using other back ends and target sources (sqlite db, REST services)
* cloud-based computing
* metadata
