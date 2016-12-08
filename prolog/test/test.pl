% * -*- Mode: Prolog -*- */

default_ref_dir("ref").
default_test_dir("target").

:- dynamic failed_test/2.
:- dynamic only_test/1.

base_path(Dir) :-
	prolog_load_context(directory,SrcDir),
	string_concat(SrcDir,"../../",Dir).
base_path(Dir) :-
	working_directory(Dir,Dir).  % default

biomake_path(Path) :-
	base_path(Dir),
	string_concat(Dir,"bin/biomake",Path).

user:prolog_exception_hook(_,
                           _, _, _) :-
        backtrace(99),
        !,
        fail.

test(N) :-
	assert(only_test(N)),
	test.

test :-
	init_counts,

	announce("FAILURE TESTS"),
	run_failure_test("-f Makefile.cyclic","test1"),
	run_failure_test("--no-backtrace -f Makefile.err","empty"),
	run_failure_test("--no-backtrace -f Makefile.tab","empty"),
	run_failure_test("--no-backtrace","missing_target"),
	run_failure_test("ref","target",["echo Up to date >uptodate"],"--no-backtrace","uptodate"),
	
	announce("PROLOG SYNTAX"),
	run_test("-p Prolog.makespec","simple_prolog"),
	run_test("-p Prolog.makespec","lower_case_variable.pltest"),
	run_test("-p Prolog.makespec","upper_case_var_assignment"),
	run_test("ref/prolog","target/prolog",["rm [hmz]*"],"",""),
	run_test("ref","target",[],"-f Makefile.translate -T Makefile.translated","Makefile.translated"),
	
	announce("BASIC GNU MAKEFILE SYNTAX"),
	run_test("simple"),
	run_test("target1"),
	run_test("target2"),
	run_test("silent"),
	run_test("one_line"),
	run_test("one_line_with_deps"),
	run_test("-f Makefile.include","inc2.test"),
	run_test("-f Makefile.include","makefile_list"),
	run_test("forced_rebuild"),

	announce("AUTOMATIC VARIABLES"),
	run_test("stem.echo"),
	run_test("first_dep"),
	run_test("all_deps"),
	run_test("subdir/target_file"),
	run_test("subdir/target_dir"),
	run_test("subdir/stem_file.txt"),
	run_test("subdir/stem_dir.txt"),
	run_test("dep_file"),
	run_test("dep_dir"),
	run_test("deps_file"),
	run_test("deps_dir"),

	announce("VARIABLES"),
	run_test("multiple.wildcards.baz"),
	run_test("vars1"),
	run_test("DEF=def","vars2"),
	run_test("ABC=123","vars3"),
	run_test("hyphenated_var"),
	run_test("unbound_var"),
	run_test("escape_dollar"),
	run_test("multi_targets_from_var"),
	run_test("append_var"),
	run_test("computed_var1"),
	run_test("computed_var2"),
	run_test("computed_var3"),
	run_test("two_lines"),
	run_test("shell_assign"),

	announce("TEXT FUNCTIONS"),
	run_test("subst"),
	run_test("patsubst"),
	run_test("substref"),
	run_test("strip"),
	run_test("findstring1"),
	run_test("findstring2"),
	run_test("filter"),
	run_test("filter_out"),
	run_test("sort"),
	run_test("word2"),
	run_test("words"),
	run_test("wordlist"),
	run_test("firstword"),
	run_test("lastword"),

	announce("FILENAME FUNCTIONS"),
	run_test("dir"),
	run_test("notdir"),
	run_test("basename"),
	run_test("suffix"),
	run_test("addsuffix"),
	run_test("addprefix"),
	run_test("join"),
	run_test("wildcard"),
	run_test("abspath"),
	run_test("realpath"),

	announce("CONDITIONAL FUNCTIONS"),
	run_test("if1"),
	run_test("if2"),
	run_test("if3"),
	run_test("if4"),
	run_test("or1"),
	run_test("or2"),
	run_test("or3"),
	run_test("and1"),
	run_test("and2"),

	announce("OTHER FUNCTIONS"),
	run_test("call"),
	run_test("shell"),
	run_test("foreach"),
	run_test("bad_function_syntax"),

	announce("MD5 CHECKSUMS"),

	% this is a test of the MD5 checksums
	run_test("ref/md5","target/md5",[],"-B -H --debug md5","hello_world"),

	% the next test fakes out the MD5 checksums... kind of hacky
	% the general idea is to test whether biomake can be tricked into NOT making a target
	% because the MD5 checksums and file sizes look correct.
	% this is really a way of checking that biomake is paying attention to the checksums,
	% while only looking at the files it generates.
	run_test("ref/md5.wrong","target/md5.wrong",["echo wrong >hello","echo wrong >world","echo wrong_wrong >hello_world"],"-H","hello_world"),

	% this next test checks that the MD5 checksums *can't* be faked out if file sizes change.
	% basically the same as the previous test, but now one of the "wrong" files (world)
	% is also the wrong length, which should trigger its rebuild - but not the rebuild of
	% hello_world, on which it depends, since that has the right length and its MD5 looks OK.
	run_test("ref/md5.len","target/md5.len",["echo wrong >hello","echo wrong length >world","echo wrong_wrong >hello_world"],"-H","hello_world"),

	announce("QUEUES"),

	% Queues are a bit under-served by tests at the moment...
	run_test("-f Makefile.queue","i.am.the.garbage.flower"),
	run_test("-f Makefile.queue --one-shell","love.will.tear.us.apart"),
	run_test("-f Makefile.queue -Q test","what.difference.does.it.make"),
	run_test("-f Makefile.queue -Q poolq","they.made.you.a.moron"),
	run_test("-f Makefile.queue -Q test -H","under.blue.moon.i.saw.you"),
	run_test("-f Makefile.queue -Q poolq -H","the.head.on.the.door"),

	announce("COMMAND-LINE OPTIONS"),

	run_test("--file=Makefile.argval","arg_equals_val"),
	run_test("-f Makefile.subdir.include -I subdir","include_dir"),
	% could do with more here
	
	announce("CONDITIONAL SYNTAX"),

	run_test("-f Makefile.cond","ifdef_true"),
	run_test("-f Makefile.cond","ifdef_false"),
	run_test("-f Makefile.cond","ifeq_true"),
	run_test("-f Makefile.cond","ifeq_false"),
	% TODO: ifndef, ifneq, nesting of includes & ifs inside reachable & unreachable clauses
	
	% All done
	report_counts,
        (   failed_test(_,_)
        ->  halt(1)
        ;   halt(0)).

init_counts :-
	nb_setval(tests,0),
	nb_setval(passed,0).

announce(X) :-
    string_chars(X,C),
    length(C,L),
    n_chars(L,'=',Bc),
    string_chars(Banner,Bc),
    format("~w~n~w~n~w~n~n",[Banner,X,Banner]).

report_counts :-
	only_test(_),
	!.

report_counts :-
	nb_getval(tests,T),
	nb_getval(passed,P),
	(P = T -> format("ok: passed ~d/~d tests~n",[P,T]);
	 (forall(failed_test(N,D),
		 format("Failed test #~d: ~w~n",[N,D])),
	  format("not ok: passed ~d/~d tests~n",[P,T]))).

run_test(Target) :-
	default_ref_dir(RefDir),
	default_test_dir(TestDir),
	report_test(RefDir,TestDir,[],"",Target,"~s",[Target]).

run_test(Args,Target) :-
	default_ref_dir(RefDir),
	default_test_dir(TestDir),
	report_test(RefDir,TestDir,[],Args,Target,"~s ~s",[Args,Target]).

run_test(RefDir,TestDir,Setup,Args,Target) :-
	report_test(RefDir,TestDir,Setup,Args,Target,"[t/~s,t/~s,~s ~s]",[RefDir,TestDir,Args,Target]).

report_test(RefDir,TestDir,Setup,Args,Target,Fmt,Vars) :-
	working_directory(CWD,CWD),
	start_test(Fmt,Vars,Desc),
	!,
	(exec_test(RefDir,TestDir,Setup,Args,Target)
         -> pass_test(Desc); fail_test(Desc)),
	working_directory(_,CWD).

report_test(_,_,_,_,_,Fmt,Vars) :-
	skip_test(Fmt,Vars).

start_test(Fmt,Vars,Desc) :-
	inc(tests),
	nb_getval(tests,T),
	(only_test(N) -> N = T; true),
	format(string(Desc),Fmt,Vars),
	format("Starting test #~d: ~s~n",[T,Desc]).

skip_test(Fmt,Vars) :-
	nb_getval(tests,T),
	format(string(Desc),Fmt,Vars),
	format("Skipping test #~d: ~s~n",[T,Desc]).

pass_test(Desc) :-
        nb_getval(tests,T),
        format("ok: passed test #~d: ~s~n~n",[T,Desc]),
	inc(passed).

fail_test(Desc) :-
        nb_getval(tests,T),
	assert(failed_test(T,Desc)),
	format("not ok: failed test #~d: ~s~n~n",[T,Desc]).

inc(Counter) :-
	nb_getval(Counter, C),
	CNew is C + 1,
	nb_setval(Counter, CNew).

make_test_path(Dir,TestPath) :-
    format(string(TestPath),"t/~s",[Dir]).

make_test_path(Dir,Target,TestPath) :-
    format(string(TestPath),"t/~s/~s",[Dir,Target]).

exec_test(RefDir,TestDir,Setup,Args,Target) :-
	make_test_path(TestDir,TestPath),
	make_test_path(TestDir,Target,TargetPath),
	biomake_path(Make),
	format(string(Exec),"~s ~s ~s",[Make,Args,Target]),
	% If no "Setup" shell commands were specified, remove the target file.
	% If Setup commands were specified, let the caller take care of this.
	(Setup = []
         -> (exists_file(TargetPath)
             -> (format("Deleting ~w~n",[TargetPath]),
                 delete_file(TargetPath))
             ; true)
         ; (forall(member(Cmd,Setup),
	          (format("~s~n",[Cmd]),
                   shell(Cmd); true)))),
	format("Running '~s' in ~s~n",[Exec,TestPath]),
	working_directory(CWD,TestPath),
	shell(Exec,Err),
	!,
	(Err = 0 -> true; format("Error code ~w~n",Err), fail),
	working_directory(_,CWD),
	compare_output(TestDir,RefDir,Target),
	% If no "Setup" shell commands were specified, remove the target file again at the end.
	(Setup = [] -> (exists_file(TargetPath) -> delete_file(TargetPath); true); true).

% If we are using the default test & reference directories,
% then just compare the target files.
compare_output(TestDir,RefDir,Target) :-
    default_test_dir(TestDir),
    default_ref_dir(RefDir),
    !,
    make_test_path(TestDir,TestPath),
    make_test_path(RefDir,RefPath),
    compare_files(TestPath,RefPath,Target).

% If we are not in the default test & reference directories,
% then compare the entire directories, allowing for more sensitive tests.
compare_output(TestDir,RefDir,_) :-
    make_test_path(TestDir,TestPath),
    make_test_path(RefDir,RefPath),
    compare_files(TestPath,RefPath).

actual_files(Dir,List) :-
    directory_files(Dir,Files),
    include(not_special,Files,List).

not_special(File) :-
    \+ special(File).
special('.').
special('..').

compare_files(TestPath,RefPath,File) :-
    format(string(TestFilePath),"~s/~s",[TestPath,File]),
    format(string(RefFilePath),"~s/~s",[RefPath,File]),
    compare_files(TestFilePath,RefFilePath).

% Directory version of compare_files recursively compares directories
compare_files(TestPath,RefPath) :-
    exists_directory(TestPath),
    exists_directory(RefPath),
    !,
    format("Comparing directory ~s to ~s~n",[TestPath,RefPath]),
    actual_files(TestPath,TestFiles),
    actual_files(RefPath,RefFiles),
    (lists_equal(TestFiles,RefFiles);
     (format("File lists do not match~n~w: ~w~n~w: ~w~n",[TestPath,TestFiles,RefPath,RefFiles]),
      fail)),
    !,
    forall(member(File,TestFiles),
	   compare_files(TestPath,RefPath,File)).

% File version of compare_files tests for equality
compare_files(TestPath,RefPath) :-
    format("Comparing file ~s to ~s ... ",[TestPath,RefPath]),
    read_string_from_file(TestPath,TestText),
    read_string_from_file(RefPath,RefText),
    RefText = TestText,
    format("match~n",[TestPath,RefPath]).

% If file version of compare_files failed, but files were present, then print a diff
compare_files(TestPath,RefPath) :-
	exists_file(TestPath),
	exists_file(RefPath),
	format("MISMATCH~n",[TestPath,RefPath]),
	format(string(Diff),"diff -y ~s ~s",[TestPath,RefPath]),
	format("~s:~n",[Diff]),
	shell(Diff,_),
	fail.

% If file version of compare_files failed because a file is absent, then say so
compare_files(TestPath,_) :-
	file_missing(TestPath),
	fail.

compare_files(_,RefPath) :-
	file_missing(RefPath),
	fail.

lists_equal([],[]) :- !.
lists_equal([X|Xs],[X|Ys]) :- !, lists_equal(Xs,Ys).
    
file_missing(Path) :-
	\+ exists_file(Path),
	format("File ~s does not exist~n",[Path]).

read_string_from_file(Path,String) :-
	exists_file(Path),
	open(Path,read,IO,[]),
	read_string(IO,"","",_,String),
	close(IO).

run_failure_test(Args,Target) :-
	default_ref_dir(RefDir),
	default_test_dir(TestDir),
	report_failure_test(RefDir,TestDir,[],Args,Target,"[~s ~s] (expecting failure)",[Args,Target]).

run_failure_test(RefDir,TestDir,Setup,Args,Target) :-
        report_failure_test(RefDir,TestDir,Setup,Args,Target,"[t/~s,t/~s,~s ~s] (expecting failure)",[RefDir,TestDir,Args,Target]).

report_failure_test(RefDir,TestDir,Setup,Args,Target,Fmt,Vars) :-
	working_directory(CWD,CWD),
	start_test(Fmt,Vars,Desc),
	!,
	(exec_test(RefDir,TestDir,Setup,Args,Target)
         -> fail_test(Desc); pass_test(Desc)),
	working_directory(_,CWD).

report_failure_test(_,_,_,_,_,Fmt,Vars) :-
	skip_test(Fmt,Vars).

n_chars(N,_,[]) :- N =< 0, !.
n_chars(N,C,[C|Ls]) :- Ndec is N - 1, n_chars(Ndec,C,Ls), !.
