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
	nb_setval(tests,0),
	nb_setval(passed,0),
	run_test("hello.world.baz"),
	run_test("test1"),
	run_test("DEF=def","test2"),
	run_test("ABC=123","test3"),
	!,
	nb_getval(tests,T),
	nb_getval(passed,P),
	(P = T -> format("ok: passed ~d/~d tests~n",[P,T]);
	    format("not ok: passed ~d/~d tests~n",[P,T])),
	halt.

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
	format(string(Desc),Fmt,Vars),
	inc(tests),
	nb_getval(tests,T),
	format("Starting test #~d: ~s~n",[T,Desc]),
	(exec_test(RefDir,TestDir,Args,Target)
         -> (format("ok: passed test ~s~n~n",[Desc]),
	 inc(passed));
         format("not ok: failed test ~s~n~n",[Desc])).

inc(Counter) :-
	nb_getval(Counter, C),
	CNew is C + 1,
	nb_setval(Counter, CNew).

exec_test(RefDir,TestDir,Args,Target) :-
	plmake_path(Make),
	format(string(Exec),"~s ~s ~s",[Make,Args,Target]),
	working_directory(CWD,TestDir),
	format("Running ~s in ~s~n",[Exec,TestDir]),
	shell(Exec,Err),
	!,
	(Err = 0 -> true; format("Error ~w~n",Err), fail),
	working_directory(_,CWD),
	read_string_from_file(RefDir,Target,RefPath,RefText),
	read_string_from_file(TestDir,Target,TestPath,TestText),
	(RefText = TestText -> true;
	    (format(string(Diff),"diff ~s ~s",RefPath,TestPath),
	    shell(Diff,_),
	    fail)),
	format("~s matches ~s~n",[TestPath,RefPath]).

read_string_from_file(Dir,Filename,Path,String) :-
	format(string(Path),"~s/~s",[Dir,Filename]),
	open(Path,read,IO,[]),
	read_string(IO,"","",_,String),
	close(IO).
