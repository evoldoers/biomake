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
	run_test("hello.world.baz"),
	run_test("test1"),
	run_test("DEF=def","test2"),
	run_test("ABC=123","test3"),
	run_test("parallels"),
	run_test("hw.test"),
	run_test("-f Makefile.include","inc2.test"),
	run_test("-f Makefile.include","makefile_list"),
	run_test("firstword"),
	run_test("lastword"),
	run_test("word2"),
	run_test("words"),
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
