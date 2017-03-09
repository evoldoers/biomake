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

biomake_cmd(Args,Target,Cmd) :-
	biomake_path(Make),
	format(string(Cmd),"~s ~s ~s",[Make,Args,Target]).

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
	run_failure_test("-f Makefile.cyclic","cyclic.test1"),
	run_failure_test("--no-backtrace -f Makefile.err","empty"),
	run_failure_test("--no-backtrace -f Makefile.tab","empty"),
	run_failure_test("--no-backtrace","missing_target"),
	run_failure_test("ref","target",["echo Up to date >uptodate"],[],"--no-backtrace","uptodate"),
	
	announce("PROLOG SYNTAX"),
	run_test("-p Prolog.makespec","simple_prolog"),
	run_test("-p Prolog.makespec","lower_case_variable.pltest"),
	run_test("-p Prolog.makespec","upper_case_var_assignment"),
	run_test("ref/prolog","target/prolog",["rm [hmz]*"],[],"",""),
	run_test("ref","target",[],[],"-f Makefile.translate -T Makefile.translated","Makefile.translated"),
	
	announce("BASIC GNU MAKEFILE SYNTAX"),
	run_test("simple"),
	run_test("target1"),
	run_test("target2"),
	run_test("silent"),
	run_test("one_line"),
	run_test("one_line_with_deps"),
	run_test("comment_in_rule"),
	run_test("comment_in_deps"),
	run_test("-f Makefile.include","inc2.test"),
	run_test("-f Makefile.include","makefile_list"),
	run_test("-f Makefile.dir1","relative_include_path"),
	run_test("forced_rebuild"),
	run_test("ref","target",["touch old_dep","echo Pre-update >older_dep"],[],"","older_dep"),
	run_test("ref","target",["echo Pre-update >newer_dep","sleep 1","touch new_dep"],[],"","newer_dep"),
	run_test("altrules1.type1"),
	run_test("altrules2.type1"),
	run_failure_test("ref","target",["touch altdep1"],[],"-f Makefile.patterns","deps_exist_but_rules_fail"),
	run_test("ref","target",["touch pattern.dep"],[],"-f Makefile.patterns","pattern_deps_exist_but_rules_fail"),
	run_test("-f Makefile.patterns -B setup_always_make_with_missing_pattern_dep","always_make_with_missing_pattern_dep.test"),
	run_test("escape_dollar"),
	run_test("percent_in_body"),
	run_test("split_lines"),
	run_test("split_recipe_lines"),
	run_failure_test("--no-backtrace -f Makefile.nl","escaped_nl"),
	run_test("--no-backtrace -f Makefile.nl2","escaped_nl2"),
	run_test("wildcard_deps"),
	run_test("-f Makefile.suppress","suppress_errors_temporarily"),
	run_test("-f Makefile.suppress -Q test","suppress_errors_temporarily_in_script"),
	run_test("-f Makefile.targetexpr","braces_in_deplist"),
	run_test("-f Makefile.targetexpr","function_in_deplist"),
	run_test("-f Makefile.targetexpr","slash_var_in_deplist"),
	run_test("-f Makefile.targetexpr","var_slash_var_in_deplist"),
	run_test("-f Makefile.modifier","padded_modifier"),
	run_test("-f Makefile.modifier","padded_modifier_from_foreach"),
	run_test("-f Makefile.dep_linebreak","dep_linebreak"),

	announce("SPECIAL TARGETS"),
	run_test("-f Makefile.oneshell","oneshell"),
	run_test("ref","target",["echo untouched by biomake > oneshell_control"],[],"-f Makefile.oneshell_control","oneshell_control"),
	run_test("ref","target",["echo empty > test.phony","echo empty > test2.phony"],[],"-f Makefile.phony1","phony_target"),
	run_test("ref","target",["echo empty > test.phony","echo empty > test2.phony"],[],"-f Makefile.phony2","expanded_phony_target"),
	run_test("-f Makefile.ignore","ignore"),
	run_failure_test("-f Makefile.ignore_control","ignore_control"),
	run_test("-f Makefile.ignore_all","ignore_all"),

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
	run_test("multi_targets_from_var"),
	run_test("append_var"),
	run_test("append_simple_var"),
	run_test("append_recursive_var"),
	run_test("computed_var1"),
	run_test("computed_var2"),
	run_test("computed_var3"),
	run_test("two_lines"),
	run_test("shell_assign"),
	
	announce("CONDITIONAL SYNTAX"),
	run_test("-f Makefile.cond","ifdef_true"),
	run_test("-f Makefile.cond","ifdef_false"),
	run_test("-f Makefile.cond","ifeq_true"),
	run_test("-f Makefile.cond","ifeq_false"),
	run_test("-f Makefile.cond","ifndef_true"),
	run_test("-f Makefile.cond","ifndef_false"),
	run_test("-f Makefile.cond","ifneq_true"),
	run_test("-f Makefile.cond","ifneq_false"),
	run_test("-f Makefile.cond","ifeq_true_ifneq_false"),
	run_test("-f Makefile.cond","ifeq_false_ifneq_true"),
	run_test("-f Makefile.cond","nested_ifeq_ifneq"),
	run_test("-f Makefile.cond","nested_ifeq_include"),
	run_test("-f Makefile.cond","ifeq_space1"),
	run_test("-f Makefile.cond","ifeq_space2"),
	run_test("-f Makefile.cond","ifeq_space3"),
	run_test("-f Makefile.cond","ifeq_quote"),
	run_test("-f Makefile.cond","ifeq_dblquote"),
	run_test("-f Makefile.cond","empty_ifeq"),
	run_test("-f Makefile.cond","complex_ifeq"),

	announce("TEXT FUNCTIONS"),
	run_test("subst"),
	run_test("patsubst"),
	run_test("substref"),
	run_test("substref_list"),
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
	run_test("var_in_command"),

	announce("FILENAME FUNCTIONS"),
	run_test("dir"),
	run_test("notdir"),
	run_test("basename"),
	run_test("suffix"),
	run_test("addsuffix"),
	run_test("addprefix"),
	run_test("join"),
	run_test("wildcard"),
	run_test("wildcard_nonexistent"),
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
	run_test("value"),
	run_test("bad_function_syntax"),
	run_test("function_whose_args_are_both_expressions"),

	announce("ARITHMETIC FUNCTIONS"),
	run_test("iota"),
	run_test("iota2"),
	run_test("add"),
	run_test("multiply"),
	run_test("divide"),
	run_test("iota_add_multiply"),

	announce("MD5 CHECKSUMS"),

	% this is a low-level unit test of the MD5 checksums
	run_test("ref/md5","target/md5",[],[],"-B -H --debug md5","hello_world"),

	% this tests that the file is not rebuilt just because of modification times
	biomake_cmd("-f Makefile.md5 -H --debug md5","md5_avoid_update",MakeMd5AvoidUpdate),
	run_test("ref","target",[MakeMd5AvoidUpdate,"sleep 1","touch md5_avoid_update_dep"],[],"-f Makefile.md5 -H --debug md5 SRC=test","md5_avoid_update"),

	% the next test fakes out the MD5 checksums... kind of hacky
	% the general idea is to test whether biomake can be tricked into NOT making a target
	% because the MD5 checksums and file sizes look correct.
	% this is really a way of checking that biomake is paying attention to the checksums,
	% while only looking at the files it generates.
	run_test("ref/md5.wrong","target/md5.wrong",["echo wrong >hello","echo wrong >world","echo wrong_wrong >hello_world","sleep 1","mkdir -p .biomake/md5","cp ../md5.checksums/* .biomake/md5"],[],"-H","hello_world"),

	% this next test checks that the MD5 checksums *can't* be faked out if file sizes change.
	% basically the same as the previous test, but now one of the "wrong" files (world)
	% is also the wrong length, which should trigger its rebuild - but not the rebuild of
	% hello_world, on which it depends, since that has the right length and its MD5 looks OK.
	run_test("ref/md5.len","target/md5.len",["echo wrong >hello","echo wrong length >world","echo wrong_wrong >hello_world","sleep 1","mkdir -p .biomake/md5","cp ../md5.checksums/* .biomake/md5"],[],"-H","hello_world"),

	% this next test checks that the MD5 checksums are recomputed if the MD5 cache file modification times look stale.
	run_test("ref/md5.time","target/md5.time",["echo wrong >hello","echo wrong_wrong >hello_world","sleep 1","mkdir -p .biomake/md5","cp ../md5.checksums/* .biomake/md5","sleep 1","echo wrong >world"],[],"-H","hello_world"),

	announce("QUEUES"),

	% Queues are a bit under-served by tests at the moment...
	% The first two tests just test that the Makefile is working and that commands can be run from a script.
	run_test("-f Makefile.queue","i.am.the.garbage.flower"),
	run_test("-f Makefile.queue --one-shell","love.will.tear.us.apart"),
	% The remaining tests use the test queue (which just runs commands in a script),
	% the thread-pool queue, and a faked version of the SGE queue, with and without MD5 hashes.
	run_test("-f Makefile.queue -Q test","what.difference.does.it.make"),
	run_test("-f Makefile.queue -Q poolq","they.made.you.a.moron"),
	run_test("-f Makefile.queue -Q test -H","under.blue.moon.i.saw.you"),
	run_test("-f Makefile.queue -Q poolq -H","the.head.on.the.door"),
	run_test("ref","target",[],["t/sge/fake_qwait"],"-d -f Makefile.queue -Q sge --qsub-exec ../sge/fake_qsub --qdel-exec ../sge/fake_qdel","outside.theres.a.boxcar.waiting"),
	run_test("ref","target",[],["t/sge/fake_qwait"],"-d -f Makefile.queue -Q sge -H --qsub-exec ../sge/fake_qsub --qdel-exec ../sge/fake_qdel","that.was.my.favourite.dress"),
	run_test("ref","target",[],["t/sge/fake_qwait"],"-d -f Makefile.queue -Q sge --qsub-exec ../sge/fake_qsub --qdel-exec ../sge/fake_qdel --queue-args '--fake-arg dummy'","walk.right.through.the.door"),
	run_test("ref","target",["rm test_file"],["t/sge/fake_qwait"],"-d -f Makefile.queue -Q sge --qsub-exec ../sge/fake_qsub --qdel-exec ../sge/fake_qdel","your-own-personal-jesus"),
	run_test("ref","target",["rm test_file2"],["t/sge/fake_qwait"],"-d -f Makefile.queue -Q sge --qsub-exec ../sge/fake_qsub --qdel-exec ../sge/fake_qdel","reach-out,touch-faith"),
	run_test("ref","target",["echo 'echo testing test_file3 >test_file3' >test_header_file"],["t/sge/fake_qwait"],"-d -f Makefile.queue -Q sge --qsub-exec ../sge/fake_qsub --qdel-exec ../sge/fake_qdel","flesh_and_bone_by_the_telephone"),

	announce("COMMAND-LINE OPTIONS"),
	run_test("--file=Makefile.argval","arg_equals_val"),
	run_test("-f Makefile.subdir.include -I subdir","include_dir"),
	run_test("ref","target",["touch what_if_dep","sleep 1","echo Pre-update >what_if"],[],"-W what_if_dep","what_if"),
	run_test("ref","target",["echo Pre-update >old_file_target","sleep 1","touch old_file_target_dep"],[],"-o old_file_target","old_file_target"),
	run_test("ref","target",["echo Pre-update >old_file_dep","sleep 1","touch old_file_dep_dep"],[],"-o old_file_dep_dep","old_file_dep"),
	run_test("-k nonexistent_target","keep_going"),
	run_failure_test("another_nonexistent_target","stop_on_error1"),
	run_failure_test("-k -S yet_another_nonexistent_target","stop_on_error2"),
	run_test("ref","target",["echo Pre-update >touch"],[],"-B -t","touch"),
	run_test("ref/md5.touch","target/md5.touch",["echo wrong >hello","echo wrong >world","echo wrong_wrong >hello_world"],[],"-t -H","hello_world"),
	run_test("ref","target",["echo Pre-update >multi_arg"],[],"-Bk still_another_nonexistent_target","multi_arg"),
	run_test("CMDLINE_VAR=average --eval EVAL_VAR=worthy","cmdline_eval1"),
	run_test("CMDLINE_VAR=mediocre. --eval-prolog EVAL_VAR=deserving.","cmdline_eval2"),
	
	announce("EMBEDDED PROLOG SYNTAX"),
	run_test("-f Makefile.bagof","bagof1"),
	run_test("-f Makefile.bagof","bagof2"),
	run_test("-f Makefile.bagof","bagof3"),
	run_test("-f Makefile.goal","headgoal_a"),
	run_test("-f Makefile.goal","headgoal_x"),
	run_test("-f Makefile.goal","depgoal_a"),
	run_test("-f Makefile.goal","depgoal_x"),
	run_test("-f Makefile.goal","head_and_dep_goals_a_b"),
	run_test("-f Makefile.goal","head_and_dep_goals_a_x"),
	run_test("-f Makefile.goal","head_and_dep_goals_x_b"),
	run_test("-f Makefile.goal","head_and_dep_goals_x_y"),
	run_test("-f Makefile.goal","multiline_depgoal_c_b"),
	run_test("-f Makefile.goal","multiline_depgoal_a_x"),
	run_test("-f Makefile.goal","multiline_depgoal_x_b"),
	run_test("-f Makefile.goal","multi_matches_abc.def.ghi.jkl"),
	run_test("-f Makefile.goal","pass_var_to_prolog"),
	run_test("-f Makefile.goal","pass_var_to_prolog2"),
	run_test("-f Makefile.goal","pass_var_to_prolog3"),
	run_test("ref/embedded","target/embedded",["rm [hmz]*"],[],"",""),
	run_test("-f Makefile.precedence","rule_precedence_generic"),
	run_test("-f Makefile.precedence","rule_precedence_specific1"),
	run_test("-f Makefile.precedence","rule_precedence_specific2"),
	run_test("-f Makefile.precedence","rule_precedence_specific3"),
	run_test("-f Makefile.precedence","rule_precedence_somewhat_specific"),
	run_test("-f Makefile.precedence","rule_precedence_even_more_control"),
	run_test("-f Makefile.precedence","rule_precedence_even_more_specific"),
	run_test("-f Makefile.depchain","dep_chain_one_step"),
	run_test("-f Makefile.depchain","dep_chain_two_step"),
	run_test("-f Makefile.size_file","size_file_empty_dep"),
	run_test("-f Makefile.size_file","size_file_nonempty_dep"),
	run_test("-f Makefile.size_file -Q test","size_file_empty_test_dep"),
	run_test("-f Makefile.size_file -Q test","size_file_nonempty_test_dep"),
	run_test("-f Makefile.size_file -Q poolq","size_file_empty_poolq_dep"),
	run_test("-f Makefile.size_file -Q poolq","size_file_nonempty_poolq_dep"),
	run_test("-f Makefile.dcg","dcg_test"),
	run_test("-f Makefile.dcg","mismatch_dcg_test"),

	% All done
	report_counts,
        (   failed_test(_,_)
        ->  halt(1)
        ;   halt(0)).

init_counts :-
	nb_setval(tests,0),
	nb_setval(passed,0).

announce(_) :-
    only_test(_),
    !.

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
	report_test(RefDir,TestDir,[],[],"",Target,"~s",[Target]).

run_test(Args,Target) :-
	default_ref_dir(RefDir),
	default_test_dir(TestDir),
	report_test(RefDir,TestDir,[],[],Args,Target,"~s ~s",[Args,Target]).

run_test(RefDir,TestDir,Setup,Cleanup,Args,Target) :-
	report_test(RefDir,TestDir,Setup,Cleanup,Args,Target,"[t/~s,t/~s,~s ~s]",[RefDir,TestDir,Args,Target]).

report_test(RefDir,TestDir,Setup,Cleanup,Args,Target,Fmt,Vars) :-
	working_directory(CWD,CWD),
	start_test(Fmt,Vars,Desc),
	!,
	(exec_test(RefDir,TestDir,Setup,Cleanup,Args,Target)
         -> pass_test(Desc); fail_test(Desc)),
	working_directory(_,CWD).

report_test(_,_,_,_,_,_,_,_).

start_test(Fmt,Vars,Desc) :-
	inc(tests),
	nb_getval(tests,T),
	(only_test(N) -> N = T; true),
	format(string(Desc),Fmt,Vars),
	format("Starting test #~d: ~s~n",[T,Desc]).

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

exec_test(RefDir,TestDir,Setup,Cleanup,Args,Target) :-
	make_test_path(TestDir,TestPath),
	make_test_path(TestDir,Target,TargetPath),
	biomake_cmd(Args,Target,Exec),
	working_directory(CWD,TestPath),
	% If no "Setup" shell commands were specified, remove the target file.
	% If Setup commands were specified, let the caller take care of this.
	(Setup = []
         -> (exists_file(Target)
             -> (format("Deleting ~w~n",[Target]),
                 delete_file(Target))
             ; true)
         ; (forall(member(Cmd,Setup),
	          (format("~s~n",[Cmd]),
                   shell(Cmd); true)))),
	format("Running '~s' in ~s~n",[Exec,TestPath]),
	shell(Exec,Err),
	!,
	(Err = 0 -> true; format("Error code ~w~n",Err), fail),
	working_directory(_,CWD),
	% 'Cleanup' is a bit of a misnomer, it's more like a post-processing step
	forall(member(Cmd,Cleanup),
	       (format("~s~n",[Cmd]),
               shell(Cmd); true)),
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

non_ignored_files(Dir,List) :-
    directory_files(Dir,Files),
    include(not_ignored,Files,List).

not_ignored(File) :-
    \+ ignored(File).
ignored('.').
ignored('..').
ignored('tmp').

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
    non_ignored_files(TestPath,TestFiles),
    non_ignored_files(RefPath,RefFiles),
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
	report_failure_test(RefDir,TestDir,[],[],Args,Target,"[~s ~s] (expecting failure)",[Args,Target]).

run_failure_test(RefDir,TestDir,Setup,Cleanup,Args,Target) :-
        report_failure_test(RefDir,TestDir,Setup,Cleanup,Args,Target,"[t/~s,t/~s,~s ~s] (expecting failure)",[RefDir,TestDir,Args,Target]).

report_failure_test(RefDir,TestDir,Setup,Cleanup,Args,Target,Fmt,Vars) :-
	working_directory(CWD,CWD),
	start_test(Fmt,Vars,Desc),
	!,
	(exec_test(RefDir,TestDir,Setup,Cleanup,Args,Target)
         -> fail_test(Desc); pass_test(Desc)),
	working_directory(_,CWD).

report_failure_test(_,_,_,_,_,_,_,_).

n_chars(N,_,[]) :- N =< 0, !.
n_chars(N,C,[C|Ls]) :- Ndec is N - 1, n_chars(Ndec,C,Ls), !.
