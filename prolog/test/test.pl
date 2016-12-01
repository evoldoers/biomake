% * -*- Mode: Prolog -*- */

default_ref_dir("t/ref").
default_test_dir("t/target").

base_path(Dir) :-
	prolog_load_context(directory,SrcDir),
	string_concat(SrcDir,"../../",Dir).
base_path(Dir) :-
	working_directory(Dir,Dir).  % default

plmake_path(Path) :-
	base_path(Dir),
	string_concat(Dir,"bin/plmake",Path).

user:prolog_exception_hook(_,
                           _, _, _) :-
        backtrace(99),
        !,
        fail.

test :-
	init_counts,
	run_test("-p Prolog.makespec","simple_prolog"),
	run_test("simple"),
	run_test("target1"),
	run_test("target2"),
	run_test("stem.echo"),
	run_test("first_dep"),
	run_test("all_deps"),
	run_test("multiple.wildcards.baz"),
	run_test("silent"),
	run_test("one_line"),
	run_test("one_line_with_deps"),
	run_test("vars1"),
	run_test("DEF=def","vars2"),
	run_test("ABC=123","vars3"),
	run_test("escape_dollar"),
	run_test("multi_targets_from_var"),
	run_test("append_var"),
	run_test("-f Makefile.include","inc2.test"),
	run_test("-f Makefile.include","makefile_list"),
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
	run_test("computed_var1"),
	run_test("computed_var2"),
	run_test("computed_var3"),
	run_test("two_lines"),
	run_test("call"),
	run_test("shell"),
	run_test("shell_assign"),
	run_test("subdir/target_file"),
	run_test("subdir/target_dir"),
	run_test("subdir/stem_file.txt"),
	run_test("subdir/stem_dir.txt"),
	run_test("dep_file"),
	run_test("dep_dir"),
	run_test("deps_file"),
	run_test("deps_dir"),
	run_test("forced_rebuild_setup"),
	run_test("forced_rebuild"),
	report_counts,
	halt.

init_counts :-
	nb_setval(tests,0),
	nb_setval(passed,0).

report_counts :-
	nb_getval(tests,T),
	nb_getval(passed,P),
	(P = T -> format("ok: passed ~d/~d tests~n",[P,T]);
	    format("not ok: passed ~d/~d tests~n",[P,T])).

run_test(Target) :-
	default_ref_dir(RefDir),
	default_test_dir(TestDir),
	report_test(RefDir,TestDir,"",Target,"~s",[Target]).

run_test(Args,Target) :-
	default_ref_dir(RefDir),
	default_test_dir(TestDir),
	report_test(RefDir,TestDir,Args,Target,"~s ~s",[Args,Target]).

run_test(RefDir,TestDir,Args,Target) :-
	report_test(RefDir,TestDir,Args,Target,"[~s,~s,~s ~s]",[RefDir,TestDir,Args,Target]).

report_test(RefDir,TestDir,Args,Target,Fmt,Vars) :-
	working_directory(CWD,CWD),
	format(string(Desc),Fmt,Vars),
	inc(tests),
	nb_getval(tests,T),
	format("Starting test #~d: ~s~n",[T,Desc]),
	(exec_test(RefDir,TestDir,Args,Target)
         -> (format("ok: passed test #~d: ~s~n~n",[T,Desc]),
	 inc(passed));
         format("not ok: failed test #~d: ~s~n~n",[T,Desc])),
	working_directory(_,CWD).

inc(Counter) :-
	nb_getval(Counter, C),
	CNew is C + 1,
	nb_setval(Counter, CNew).

exec_test(RefDir,TestDir,Args,Target) :-
	format(string(TestPath),"~s/~s",[TestDir,Target]),
	format(string(RefPath),"~s/~s",[RefDir,Target]),
	(exists_file(TestPath) -> delete_file(TestPath); true),
	plmake_path(Make),
	format(string(Exec),"~s -B ~s ~s",[Make,Args,Target]),
	working_directory(CWD,TestDir),
	format("Running '~s' in ~s~n",[Exec,TestDir]),
	shell(Exec,Err),
	!,
	(Err = 0 -> true; format("Error ~w~n",Err), fail),
	working_directory(_,CWD),
	compare_files(TestPath,RefPath),
	delete_file(TestPath).

compare_files(TestPath,RefPath) :-
	format("Comparing ~s to ~s~n",[TestPath,RefPath]),
	read_string_from_file(TestPath,TestText),
	read_string_from_file(RefPath,RefText),
	RefText = TestText,
	format("~s matches ~s~n",[TestPath,RefPath]).

compare_files(TestPath,RefPath) :-
	exists_file(TestPath),
	exists_file(RefPath),
	format("~s does not match ~s~n",[TestPath,RefPath]),
	format(string(Diff),"diff -y ~s ~s",[TestPath,RefPath]),
	format("~s:~n",[Diff]),
	shell(Diff,_),
	fail.

compare_files(TestPath,_) :-
	file_missing(TestPath),
	fail.

compare_files(_,RefPath) :-
	file_missing(RefPath),
	fail.

file_missing(Path) :-
	\+ exists_file(Path),
	format("File ~s does not exist~n",[Path]).

read_string_from_file(Path,String) :-
	exists_file(Path),
	open(Path,read,IO,[]),
	read_string(IO,"","",_,String),
	close(IO).
