#!/bin/sh

test_description='blob conversion via gitattributes'

. ./test-lib.sh

TEST_ROOT="$PWD"
PATH=$TEST_ROOT:$PATH

write_script <<\EOF "$TEST_ROOT/rot13.sh"
tr \
  'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' \
  'nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM'
EOF

write_script rot13-filter.pl "$PERL_PATH" \
	<"$TEST_DIRECTORY"/t0021/rot13-filter.pl

generate_random_characters () {
	LEN=$1
	NAME=$2
	test-tool genrandom some-seed $LEN |
		perl -pe "s/./chr((ord($&) % 26) + ord('a'))/sge" >"$TEST_ROOT/$NAME"
}

file_size () {
	test-tool path-utils file-size "$1"
}

filter_git () {
	rm -f *.log &&
	git "$@"
}

# Compare two files and ensure that `clean` and `smudge` respectively are
# called at least once if specified in the `expect` file. The actual
# invocation count is not relevant because their number can vary.
# c.f. http://lore.kernel.org/git/xmqqshv18i8i.fsf@gitster.mtv.corp.google.com/
test_cmp_count () {
	expect=$1
	actual=$2
	for FILE in "$expect" "$actual"
	do
		sort "$FILE" | uniq -c |
		sed -e "s/^ *[0-9][0-9]*[ 	]*IN: /x IN: /" >"$FILE.tmp"
	done &&
	test_cmp "$expect.tmp" "$actual.tmp" &&
	rm "$expect.tmp" "$actual.tmp"
}

# Compare two files but exclude all `clean` invocations because Git can
# call `clean` zero or more times.
# c.f. http://lore.kernel.org/git/xmqqshv18i8i.fsf@gitster.mtv.corp.google.com/
test_cmp_exclude_clean () {
	expect=$1
	actual=$2
	for FILE in "$expect" "$actual"
	do
		grep -v "IN: clean" "$FILE" >"$FILE.tmp"
	done &&
	test_cmp "$expect.tmp" "$actual.tmp" &&
	rm "$expect.tmp" "$actual.tmp"
}

# Check that the contents of two files are equal and that their rot13 version
# is equal to the committed content.
test_cmp_committed_rot13 () {
	test_cmp "$1" "$2" &&
	rot13.sh <"$1" >expected &&
	git cat-file blob :"$2" >actual &&
	test_cmp expected actual
}

test_expect_success setup '
	git config filter.rot13.smudge ./rot13.sh &&
	git config filter.rot13.clean ./rot13.sh &&

	{
	    echo "*.t filter=rot13"
	    echo "*.i ident"
	} >.gitattributes &&

	{
	    echo a b c d e f g h i j k l m
	    echo n o p q r s t u v w x y z
	    echo '\''$Id$'\''
	} >test &&
	cat test >test.t &&
	cat test >test.o &&
	cat test >test.i &&
	git add test test.t test.i &&
	rm -f test test.t test.i &&
	git checkout -- test test.t test.i &&

	echo "content-test2" >test2.o &&
	echo "content-test3 - filename with special characters" >"test3 '\''sq'\'',\$x=.o"
'

script='s/^\$Id: \([0-9a-f]*\) \$/\1/p'

test_expect_success check '

	test_cmp test.o test &&
	test_cmp test.o test.t &&

	# ident should be stripped in the repository
	git diff --raw --exit-code :test :test.i &&
	id=$(git rev-parse --verify :test) &&
	embedded=$(sed -ne "$script" test.i) &&
	test "z$id" = "z$embedded" &&

	git cat-file blob :test.t >test.r &&

	./rot13.sh <test.o >test.t &&
	test_cmp test.r test.t
'

# If an expanded ident ever gets into the repository, we want to make sure that
# it is collapsed before being expanded again on checkout
test_expect_success expanded_in_repo '
	{
		echo "File with expanded keywords"
		echo "\$Id\$"
		echo "\$Id:\$"
		echo "\$Id: 0000000000000000000000000000000000000000 \$"
		echo "\$Id: NoSpaceAtEnd\$"
		echo "\$Id:NoSpaceAtFront \$"
		echo "\$Id:NoSpaceAtEitherEnd\$"
		echo "\$Id: NoTerminatingSymbol"
		echo "\$Id: Foreign Commit With Spaces \$"
	} >expanded-keywords.0 &&

	{
		cat expanded-keywords.0 &&
		printf "\$Id: NoTerminatingSymbolAtEOF"
	} >expanded-keywords &&
	cat expanded-keywords >expanded-keywords-crlf &&
	git add expanded-keywords expanded-keywords-crlf &&
	git commit -m "File with keywords expanded" &&
	id=$(git rev-parse --verify :expanded-keywords) &&

	{
		echo "File with expanded keywords"
		echo "\$Id: $id \$"
		echo "\$Id: $id \$"
		echo "\$Id: $id \$"
		echo "\$Id: $id \$"
		echo "\$Id: $id \$"
		echo "\$Id: $id \$"
		echo "\$Id: NoTerminatingSymbol"
		echo "\$Id: Foreign Commit With Spaces \$"
	} >expected-output.0 &&
	{
		cat expected-output.0 &&
		printf "\$Id: NoTerminatingSymbolAtEOF"
	} >expected-output &&
	{
		append_cr <expected-output.0 &&
		printf "\$Id: NoTerminatingSymbolAtEOF"
	} >expected-output-crlf &&
	{
		echo "expanded-keywords ident"
		echo "expanded-keywords-crlf ident text eol=crlf"
	} >>.gitattributes &&

	rm -f expanded-keywords expanded-keywords-crlf &&

	git checkout -- expanded-keywords &&
	test_cmp expected-output expanded-keywords &&

	git checkout -- expanded-keywords-crlf &&
	test_cmp expected-output-crlf expanded-keywords-crlf
'

# The use of %f in a filter definition is expanded to the path to
# the filename being smudged or cleaned.  It must be shell escaped.
# First, set up some interesting file names and pet them in
# .gitattributes.
test_expect_success 'filter shell-escaped filenames' '
	cat >argc.sh <<-EOF &&
	#!$SHELL_PATH
	cat >/dev/null
	echo argc: \$# "\$@"
	EOF
	normal=name-no-magic &&
	special="name  with '\''sq'\'' and \$x" &&
	echo some test text >"$normal" &&
	echo some test text >"$special" &&
	git add "$normal" "$special" &&
	git commit -q -m "add files" &&
	echo "name* filter=argc" >.gitattributes &&

	# delete the files and check them out again, using a smudge filter
	# that will count the args and echo the command-line back to us
	test_config filter.argc.smudge "sh ./argc.sh %f" &&
	rm "$normal" "$special" &&
	git checkout -- "$normal" "$special" &&

	# make sure argc.sh counted the right number of args
	echo "argc: 1 $normal" >expect &&
	test_cmp expect "$normal" &&
	echo "argc: 1 $special" >expect &&
	test_cmp expect "$special" &&

	# do the same thing, but with more args in the filter expression
	test_config filter.argc.smudge "sh ./argc.sh %f --my-extra-arg" &&
	rm "$normal" "$special" &&
	git checkout -- "$normal" "$special" &&

	# make sure argc.sh counted the right number of args
	echo "argc: 2 $normal --my-extra-arg" >expect &&
	test_cmp expect "$normal" &&
	echo "argc: 2 $special --my-extra-arg" >expect &&
	test_cmp expect "$special" &&
	:
'

test_expect_success 'required filter should filter data' '
	test_config filter.required.smudge ./rot13.sh &&
	test_config filter.required.clean ./rot13.sh &&
	test_config filter.required.required true &&

	echo "*.r filter=required" >.gitattributes &&

	cat test.o >test.r &&
	git add test.r &&

	rm -f test.r &&
	git checkout -- test.r &&
	test_cmp test.o test.r &&

	./rot13.sh <test.o >expected &&
	git cat-file blob :test.r >actual &&
	test_cmp expected actual
'

test_expect_success 'required filter smudge failure' '
	test_config filter.failsmudge.smudge false &&
	test_config filter.failsmudge.clean cat &&
	test_config filter.failsmudge.required true &&

	echo "*.fs filter=failsmudge" >.gitattributes &&

	echo test >test.fs &&
	git add test.fs &&
	rm -f test.fs &&
	test_must_fail git checkout -- test.fs
'

test_expect_success 'required filter clean failure' '
	test_config filter.failclean.smudge cat &&
	test_config filter.failclean.clean false &&
	test_config filter.failclean.required true &&

	echo "*.fc filter=failclean" >.gitattributes &&

	echo test >test.fc &&
	test_must_fail git add test.fc
'

test_expect_success 'filtering large input to small output should use little memory' '
	test_config filter.devnull.clean "cat >/dev/null" &&
	test_config filter.devnull.required true &&
	for i in $(test_seq 1 30); do printf "%1048576d" 1; done >30MB &&
	echo "30MB filter=devnull" >.gitattributes &&
	GIT_MMAP_LIMIT=1m GIT_ALLOC_LIMIT=1m git add 30MB
'

test_expect_success 'filter that does not read is fine' '
	test-tool genrandom foo $((128 * 1024 + 1)) >big &&
	echo "big filter=epipe" >.gitattributes &&
	test_config filter.epipe.clean "echo xyzzy" &&
	git add big &&
	git cat-file blob :big >actual &&
	echo xyzzy >expect &&
	test_cmp expect actual
'

test_expect_success EXPENSIVE 'filter large file' '
	test_config filter.largefile.smudge cat &&
	test_config filter.largefile.clean cat &&
	for i in $(test_seq 1 2048); do printf "%1048576d" 1; done >2GB &&
	echo "2GB filter=largefile" >.gitattributes &&
	git add 2GB 2>err &&
	test_must_be_empty err &&
	rm -f 2GB &&
	git checkout -- 2GB 2>err &&
	test_must_be_empty err
'

test_expect_success "filter: clean empty file" '
	test_config filter.in-repo-header.clean  "echo cleaned && cat" &&
	test_config filter.in-repo-header.smudge "sed 1d" &&

	echo "empty-in-worktree    filter=in-repo-header" >>.gitattributes &&
	>empty-in-worktree &&

	echo cleaned >expected &&
	git add empty-in-worktree &&
	git show :empty-in-worktree >actual &&
	test_cmp expected actual
'

test_expect_success "filter: smudge empty file" '
	test_config filter.empty-in-repo.clean "cat >/dev/null" &&
	test_config filter.empty-in-repo.smudge "echo smudged && cat" &&

	echo "empty-in-repo filter=empty-in-repo" >>.gitattributes &&
	echo dead data walking >empty-in-repo &&
	git add empty-in-repo &&

	echo smudged >expected &&
	git checkout-index --prefix=filtered- empty-in-repo &&
	test_cmp expected filtered-empty-in-repo
'

test_expect_success 'disable filter with empty override' '
	test_config_global filter.disable.smudge false &&
	test_config_global filter.disable.clean false &&
	test_config filter.disable.smudge false &&
	test_config filter.disable.clean false &&

	echo "*.disable filter=disable" >.gitattributes &&

	echo test >test.disable &&
	git -c filter.disable.clean= add test.disable 2>err &&
	test_must_be_empty err &&
	rm -f test.disable &&
	git -c filter.disable.smudge= checkout -- test.disable 2>err &&
	test_must_be_empty err
'

test_expect_success 'diff does not reuse worktree files that need cleaning' '
	test_config filter.counter.clean "echo . >>count; sed s/^/clean:/" &&
	echo "file filter=counter" >.gitattributes &&
	test_commit one file &&
	test_commit two file &&

	>count &&
	git diff-tree -p HEAD &&
	test_line_count = 0 count
'

test_expect_success PERL 'required process filter should filter data' '
	test_config_global filter.protocol.process "rot13-filter.pl debug.log clean smudge" &&
	test_config_global filter.protocol.required true &&
	rm -rf repo &&
	mkdir repo &&
	(
		cd repo &&
		git init &&

		echo "*.r filter=protocol" >.gitattributes &&
		git add . &&
		git commit -m "test commit 1" &&
		git branch empty-branch &&

		cp "$TEST_ROOT/test.o" test.r &&
		cp "$TEST_ROOT/test2.o" test2.r &&
		mkdir testsubdir &&
		cp "$TEST_ROOT/test3 '\''sq'\'',\$x=.o" "testsubdir/test3 '\''sq'\'',\$x=.r" &&
		>test4-empty.r &&

		S=$(file_size test.r) &&
		S2=$(file_size test2.r) &&
		S3=$(file_size "testsubdir/test3 '\''sq'\'',\$x=.r") &&
		M=$(git hash-object test.r) &&
		M2=$(git hash-object test2.r) &&
		M3=$(git hash-object "testsubdir/test3 '\''sq'\'',\$x=.r") &&
		EMPTY=$(git hash-object /dev/null) &&

		filter_git add . &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: clean test.r $S [OK] -- OUT: $S . [OK]
			IN: clean test2.r $S2 [OK] -- OUT: $S2 . [OK]
			IN: clean test4-empty.r 0 [OK] -- OUT: 0  [OK]
			IN: clean testsubdir/test3 '\''sq'\'',\$x=.r $S3 [OK] -- OUT: $S3 . [OK]
			STOP
		EOF
		test_cmp_count expected.log debug.log &&

		git commit -m "test commit 2" &&
		MASTER=$(git rev-parse --verify master) &&
		META="ref=refs/heads/master treeish=$MASTER" &&
		rm -f test2.r "testsubdir/test3 '\''sq'\'',\$x=.r" &&

		filter_git checkout --quiet --no-progress . &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: smudge test2.r blob=$M2 $S2 [OK] -- OUT: $S2 . [OK]
			IN: smudge testsubdir/test3 '\''sq'\'',\$x=.r blob=$M3 $S3 [OK] -- OUT: $S3 . [OK]
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log &&

		# Make sure that the file appears dirty, so checkout below has to
		# run the configured filter.
		touch test.r &&
		filter_git checkout --quiet --no-progress empty-branch &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: clean test.r $S [OK] -- OUT: $S . [OK]
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log &&

		filter_git checkout --quiet --no-progress master &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: smudge test.r $META blob=$M $S [OK] -- OUT: $S . [OK]
			IN: smudge test2.r $META blob=$M2 $S2 [OK] -- OUT: $S2 . [OK]
			IN: smudge test4-empty.r $META blob=$EMPTY 0 [OK] -- OUT: 0  [OK]
			IN: smudge testsubdir/test3 '\''sq'\'',\$x=.r $META blob=$M3 $S3 [OK] -- OUT: $S3 . [OK]
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log &&

		test_cmp_committed_rot13 "$TEST_ROOT/test.o" test.r &&
		test_cmp_committed_rot13 "$TEST_ROOT/test2.o" test2.r &&
		test_cmp_committed_rot13 "$TEST_ROOT/test3 '\''sq'\'',\$x=.o" "testsubdir/test3 '\''sq'\'',\$x=.r"
	)
'

test_expect_success PERL 'required process filter should filter data for various subcommands' '
	test_config_global filter.protocol.process "rot13-filter.pl debug.log clean smudge" &&
	test_config_global filter.protocol.required true &&
	(
		cd repo &&

		S=$(file_size test.r) &&
		S2=$(file_size test2.r) &&
		S3=$(file_size "testsubdir/test3 '\''sq'\'',\$x=.r") &&
		M=$(git hash-object test.r) &&
		M2=$(git hash-object test2.r) &&
		M3=$(git hash-object "testsubdir/test3 '\''sq'\'',\$x=.r") &&
		EMPTY=$(git hash-object /dev/null) &&

		MASTER=$(git rev-parse --verify master) &&

		cp "$TEST_ROOT/test.o" test5.r &&
		git add test5.r &&
		git commit -m "test commit 3" &&
		git checkout empty-branch &&
		filter_git rebase --onto empty-branch master^^ master &&
		MASTER2=$(git rev-parse --verify master) &&
		META="ref=refs/heads/master treeish=$MASTER2" &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: smudge test.r $META blob=$M $S [OK] -- OUT: $S . [OK]
			IN: smudge test2.r $META blob=$M2 $S2 [OK] -- OUT: $S2 . [OK]
			IN: smudge test4-empty.r $META blob=$EMPTY 0 [OK] -- OUT: 0  [OK]
			IN: smudge test5.r $META blob=$M $S [OK] -- OUT: $S . [OK]
			IN: smudge testsubdir/test3 '\''sq'\'',\$x=.r $META blob=$M3 $S3 [OK] -- OUT: $S3 . [OK]
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log &&

		git reset --hard empty-branch &&
		filter_git reset --hard $MASTER &&
		META="treeish=$MASTER" &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: smudge test.r $META blob=$M $S [OK] -- OUT: $S . [OK]
			IN: smudge test2.r $META blob=$M2 $S2 [OK] -- OUT: $S2 . [OK]
			IN: smudge test4-empty.r $META blob=$EMPTY 0 [OK] -- OUT: 0  [OK]
			IN: smudge testsubdir/test3 '\''sq'\'',\$x=.r $META blob=$M3 $S3 [OK] -- OUT: $S3 . [OK]
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log &&

		git branch old-master $MASTER &&
		git reset --hard empty-branch &&
		filter_git reset --hard old-master &&
		META="ref=refs/heads/old-master treeish=$MASTER" &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: smudge test.r $META blob=$M $S [OK] -- OUT: $S . [OK]
			IN: smudge test2.r $META blob=$M2 $S2 [OK] -- OUT: $S2 . [OK]
			IN: smudge test4-empty.r $META blob=$EMPTY 0 [OK] -- OUT: 0  [OK]
			IN: smudge testsubdir/test3 '\''sq'\'',\$x=.r $META blob=$M3 $S3 [OK] -- OUT: $S3 . [OK]
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log &&

		git checkout -b merge empty-branch &&
		git branch -f master $MASTER2 &&
		filter_git merge master &&
		META="treeish=$MASTER2" &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: smudge test.r $META blob=$M $S [OK] -- OUT: $S . [OK]
			IN: smudge test2.r $META blob=$M2 $S2 [OK] -- OUT: $S2 . [OK]
			IN: smudge test4-empty.r $META blob=$EMPTY 0 [OK] -- OUT: 0  [OK]
			IN: smudge test5.r $META blob=$M $S [OK] -- OUT: $S . [OK]
			IN: smudge testsubdir/test3 '\''sq'\'',\$x=.r $META blob=$M3 $S3 [OK] -- OUT: $S3 . [OK]
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log &&

		filter_git archive master >/dev/null &&
		META="ref=refs/heads/master treeish=$MASTER2" &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: smudge test.r $META blob=$M $S [OK] -- OUT: $S . [OK]
			IN: smudge test2.r $META blob=$M2 $S2 [OK] -- OUT: $S2 . [OK]
			IN: smudge test4-empty.r $META blob=$EMPTY 0 [OK] -- OUT: 0  [OK]
			IN: smudge test5.r $META blob=$M $S [OK] -- OUT: $S . [OK]
			IN: smudge testsubdir/test3 '\''sq'\'',\$x=.r $META blob=$M3 $S3 [OK] -- OUT: $S3 . [OK]
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log &&

		TREE="$(git rev-parse $MASTER2^{tree})" &&
		filter_git archive $TREE >/dev/null &&
		META="treeish=$TREE" &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: smudge test.r $META blob=$M $S [OK] -- OUT: $S . [OK]
			IN: smudge test2.r $META blob=$M2 $S2 [OK] -- OUT: $S2 . [OK]
			IN: smudge test4-empty.r $META blob=$EMPTY 0 [OK] -- OUT: 0  [OK]
			IN: smudge test5.r $META blob=$M $S [OK] -- OUT: $S . [OK]
			IN: smudge testsubdir/test3 '\''sq'\'',\$x=.r $META blob=$M3 $S3 [OK] -- OUT: $S3 . [OK]
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log
	)
'

test_expect_success PERL 'required process filter takes precedence' '
	test_config_global filter.protocol.clean false &&
	test_config_global filter.protocol.process "rot13-filter.pl debug.log clean" &&
	test_config_global filter.protocol.required true &&
	rm -rf repo &&
	mkdir repo &&
	(
		cd repo &&
		git init &&

		echo "*.r filter=protocol" >.gitattributes &&
		cp "$TEST_ROOT/test.o" test.r &&
		S=$(file_size test.r) &&

		# Check that the process filter is invoked here
		filter_git add . &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: clean test.r $S [OK] -- OUT: $S . [OK]
			STOP
		EOF
		test_cmp_count expected.log debug.log
	)
'

test_expect_success PERL 'required process filter should be used only for "clean" operation only' '
	test_config_global filter.protocol.process "rot13-filter.pl debug.log clean" &&
	rm -rf repo &&
	mkdir repo &&
	(
		cd repo &&
		git init &&

		echo "*.r filter=protocol" >.gitattributes &&
		cp "$TEST_ROOT/test.o" test.r &&
		S=$(file_size test.r) &&

		filter_git add . &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: clean test.r $S [OK] -- OUT: $S . [OK]
			STOP
		EOF
		test_cmp_count expected.log debug.log &&

		rm test.r &&

		filter_git checkout --quiet --no-progress . &&
		# If the filter would be used for "smudge", too, we would see
		# "IN: smudge test.r 57 [OK] -- OUT: 57 . [OK]" here
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log
	)
'

test_expect_success PERL 'required process filter should process multiple packets' '
	test_config_global filter.protocol.process "rot13-filter.pl debug.log clean smudge" &&
	test_config_global filter.protocol.required true &&

	rm -rf repo &&
	mkdir repo &&
	(
		cd repo &&
		git init &&

		# Generate data requiring 1, 2, 3 packets
		S=65516 && # PKTLINE_DATA_MAXLEN -> Maximal size of a packet
		generate_random_characters $(($S    )) 1pkt_1__.file &&
		generate_random_characters $(($S  +1)) 2pkt_1+1.file &&
		generate_random_characters $(($S*2-1)) 2pkt_2-1.file &&
		generate_random_characters $(($S*2  )) 2pkt_2__.file &&
		generate_random_characters $(($S*2+1)) 3pkt_2+1.file &&

		for FILE in "$TEST_ROOT"/*.file
		do
			cp "$FILE" . &&
			rot13.sh <"$FILE" >"$FILE.rot13"
		done &&

		echo "*.file filter=protocol" >.gitattributes &&
		filter_git add *.file .gitattributes &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: clean 1pkt_1__.file $(($S    )) [OK] -- OUT: $(($S    )) . [OK]
			IN: clean 2pkt_1+1.file $(($S  +1)) [OK] -- OUT: $(($S  +1)) .. [OK]
			IN: clean 2pkt_2-1.file $(($S*2-1)) [OK] -- OUT: $(($S*2-1)) .. [OK]
			IN: clean 2pkt_2__.file $(($S*2  )) [OK] -- OUT: $(($S*2  )) .. [OK]
			IN: clean 3pkt_2+1.file $(($S*2+1)) [OK] -- OUT: $(($S*2+1)) ... [OK]
			STOP
		EOF
		test_cmp_count expected.log debug.log &&

		M1="blob=$(git hash-object 1pkt_1__.file)" &&
		M2="blob=$(git hash-object 2pkt_1+1.file)" &&
		M3="blob=$(git hash-object 2pkt_2-1.file)" &&
		M4="blob=$(git hash-object 2pkt_2__.file)" &&
		M5="blob=$(git hash-object 3pkt_2+1.file)" &&
		rm -f *.file debug.log &&

		filter_git checkout --quiet --no-progress -- *.file &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: smudge 1pkt_1__.file $M1 $(($S    )) [OK] -- OUT: $(($S    )) . [OK]
			IN: smudge 2pkt_1+1.file $M2 $(($S  +1)) [OK] -- OUT: $(($S  +1)) .. [OK]
			IN: smudge 2pkt_2-1.file $M3 $(($S*2-1)) [OK] -- OUT: $(($S*2-1)) .. [OK]
			IN: smudge 2pkt_2__.file $M4 $(($S*2  )) [OK] -- OUT: $(($S*2  )) .. [OK]
			IN: smudge 3pkt_2+1.file $M5 $(($S*2+1)) [OK] -- OUT: $(($S*2+1)) ... [OK]
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log &&

		for FILE in *.file
		do
			test_cmp_committed_rot13 "$TEST_ROOT/$FILE" $FILE
		done
	)
'

test_expect_success PERL 'required process filter with clean error should fail' '
	test_config_global filter.protocol.process "rot13-filter.pl debug.log clean smudge" &&
	test_config_global filter.protocol.required true &&
	rm -rf repo &&
	mkdir repo &&
	(
		cd repo &&
		git init &&

		echo "*.r filter=protocol" >.gitattributes &&

		cp "$TEST_ROOT/test.o" test.r &&
		echo "this is going to fail" >clean-write-fail.r &&
		echo "content-test3-subdir" >test3.r &&

		test_must_fail git add .
	)
'

test_expect_success PERL 'process filter should restart after unexpected write failure' '
	test_config_global filter.protocol.process "rot13-filter.pl debug.log clean smudge" &&
	rm -rf repo &&
	mkdir repo &&
	(
		cd repo &&
		git init &&

		echo "*.r filter=protocol" >.gitattributes &&

		cp "$TEST_ROOT/test.o" test.r &&
		cp "$TEST_ROOT/test2.o" test2.r &&
		echo "this is going to fail" >smudge-write-fail.o &&
		cp smudge-write-fail.o smudge-write-fail.r &&

		S=$(file_size test.r) &&
		S2=$(file_size test2.r) &&
		SF=$(file_size smudge-write-fail.r) &&
		M=$(git hash-object test.r) &&
		M2=$(git hash-object test2.r) &&
		MF=$(git hash-object smudge-write-fail.r) &&
		rm -f debug.log &&

		git add . &&
		rm -f *.r &&

		rm -f debug.log &&
		git checkout --quiet --no-progress . 2>git-stderr.log &&

		grep "smudge write error at" git-stderr.log &&
		test_i18ngrep "error: external filter" git-stderr.log &&

		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: smudge smudge-write-fail.r blob=$MF $SF [OK] -- [WRITE FAIL]
			START
			init handshake complete
			IN: smudge test.r blob=$M $S [OK] -- OUT: $S . [OK]
			IN: smudge test2.r blob=$M2 $S2 [OK] -- OUT: $S2 . [OK]
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log &&

		test_cmp_committed_rot13 "$TEST_ROOT/test.o" test.r &&
		test_cmp_committed_rot13 "$TEST_ROOT/test2.o" test2.r &&

		# Smudge failed
		! test_cmp smudge-write-fail.o smudge-write-fail.r &&
		rot13.sh <smudge-write-fail.o >expected &&
		git cat-file blob :smudge-write-fail.r >actual &&
		test_cmp expected actual
	)
'

test_expect_success PERL 'process filter should not be restarted if it signals an error' '
	test_config_global filter.protocol.process "rot13-filter.pl debug.log clean smudge" &&
	rm -rf repo &&
	mkdir repo &&
	(
		cd repo &&
		git init &&

		echo "*.r filter=protocol" >.gitattributes &&

		cp "$TEST_ROOT/test.o" test.r &&
		cp "$TEST_ROOT/test2.o" test2.r &&
		echo "this will cause an error" >error.o &&
		cp error.o error.r &&

		S=$(file_size test.r) &&
		S2=$(file_size test2.r) &&
		SE=$(file_size error.r) &&
		M=$(git hash-object test.r) &&
		M2=$(git hash-object test2.r) &&
		ME=$(git hash-object error.r) &&
		rm -f debug.log &&

		git add . &&
		rm -f *.r &&

		filter_git checkout --quiet --no-progress . &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: smudge error.r blob=$ME $SE [OK] -- [ERROR]
			IN: smudge test.r blob=$M $S [OK] -- OUT: $S . [OK]
			IN: smudge test2.r blob=$M2 $S2 [OK] -- OUT: $S2 . [OK]
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log &&

		test_cmp_committed_rot13 "$TEST_ROOT/test.o" test.r &&
		test_cmp_committed_rot13 "$TEST_ROOT/test2.o" test2.r &&
		test_cmp error.o error.r
	)
'

test_expect_success PERL 'process filter abort stops processing of all further files' '
	test_config_global filter.protocol.process "rot13-filter.pl debug.log clean smudge" &&
	rm -rf repo &&
	mkdir repo &&
	(
		cd repo &&
		git init &&

		echo "*.r filter=protocol" >.gitattributes &&

		cp "$TEST_ROOT/test.o" test.r &&
		cp "$TEST_ROOT/test2.o" test2.r &&
		echo "error this blob and all future blobs" >abort.o &&
		cp abort.o abort.r &&

		M="blob=$(git hash-object abort.r)" &&
		rm -f debug.log &&
		SA=$(file_size abort.r) &&

		git add . &&
		rm -f *.r &&


		# Note: This test assumes that Git filters files in alphabetical
		# order ("abort.r" before "test.r").
		filter_git checkout --quiet --no-progress . &&
		cat >expected.log <<-EOF &&
			START
			init handshake complete
			IN: smudge abort.r $M $SA [OK] -- [ABORT]
			STOP
		EOF
		test_cmp_exclude_clean expected.log debug.log &&

		test_cmp "$TEST_ROOT/test.o" test.r &&
		test_cmp "$TEST_ROOT/test2.o" test2.r &&
		test_cmp abort.o abort.r
	)
'

test_expect_success PERL 'invalid process filter must fail (and not hang!)' '
	test_config_global filter.protocol.process cat &&
	test_config_global filter.protocol.required true &&
	rm -rf repo &&
	mkdir repo &&
	(
		cd repo &&
		git init &&

		echo "*.r filter=protocol" >.gitattributes &&

		cp "$TEST_ROOT/test.o" test.r &&
		test_must_fail git add . 2>git-stderr.log &&
		grep "expected git-filter-server" git-stderr.log
	)
'

test_expect_success PERL 'delayed checkout in process filter' '
	test_config_global filter.a.process "rot13-filter.pl a.log clean smudge delay" &&
	test_config_global filter.a.required true &&
	test_config_global filter.b.process "rot13-filter.pl b.log clean smudge delay" &&
	test_config_global filter.b.required true &&

	rm -rf repo &&
	mkdir repo &&
	(
		cd repo &&
		git init &&
		echo "*.a filter=a" >.gitattributes &&
		echo "*.b filter=b" >>.gitattributes &&
		cp "$TEST_ROOT/test.o" test.a &&
		cp "$TEST_ROOT/test.o" test-delay10.a &&
		cp "$TEST_ROOT/test.o" test-delay11.a &&
		cp "$TEST_ROOT/test.o" test-delay20.a &&
		cp "$TEST_ROOT/test.o" test-delay10.b &&
		git add . &&
		git commit -m "test commit"
	) &&

	S=$(file_size "$TEST_ROOT/test.o") &&
	PM="ref=refs/heads/master treeish=$(git -C repo rev-parse --verify master) " &&
	M="${PM}blob=$(git -C repo rev-parse --verify master:test.a)" &&
	cat >a.exp <<-EOF &&
		START
		init handshake complete
		IN: smudge test.a $M $S [OK] -- OUT: $S . [OK]
		IN: smudge test-delay10.a $M $S [OK] -- [DELAYED]
		IN: smudge test-delay11.a $M $S [OK] -- [DELAYED]
		IN: smudge test-delay20.a $M $S [OK] -- [DELAYED]
		IN: list_available_blobs test-delay10.a test-delay11.a [OK]
		IN: smudge test-delay10.a $M 0 [OK] -- OUT: $S . [OK]
		IN: smudge test-delay11.a $M 0 [OK] -- OUT: $S . [OK]
		IN: list_available_blobs test-delay20.a [OK]
		IN: smudge test-delay20.a $M 0 [OK] -- OUT: $S . [OK]
		IN: list_available_blobs [OK]
		STOP
	EOF
	cat >b.exp <<-EOF &&
		START
		init handshake complete
		IN: smudge test-delay10.b $M $S [OK] -- [DELAYED]
		IN: list_available_blobs test-delay10.b [OK]
		IN: smudge test-delay10.b $M 0 [OK] -- OUT: $S . [OK]
		IN: list_available_blobs [OK]
		STOP
	EOF

	rm -rf repo-cloned &&
	filter_git clone repo repo-cloned &&
	test_cmp_count a.exp repo-cloned/a.log &&
	test_cmp_count b.exp repo-cloned/b.log &&

	(
		cd repo-cloned &&
		test_cmp_committed_rot13 "$TEST_ROOT/test.o" test.a &&
		test_cmp_committed_rot13 "$TEST_ROOT/test.o" test-delay10.a &&
		test_cmp_committed_rot13 "$TEST_ROOT/test.o" test-delay11.a &&
		test_cmp_committed_rot13 "$TEST_ROOT/test.o" test-delay20.a &&
		test_cmp_committed_rot13 "$TEST_ROOT/test.o" test-delay10.b &&

		rm *.a *.b &&
		filter_git checkout . &&
		# We are not checking out a ref here, so filter out ref metadata.
		sed -e "s!$PM!!" ../a.exp >a.exp.filtered &&
		sed -e "s!$PM!!" ../b.exp >b.exp.filtered &&
		test_cmp_count a.exp.filtered a.log &&
		test_cmp_count b.exp.filtered b.log &&

		test_cmp_committed_rot13 "$TEST_ROOT/test.o" test.a &&
		test_cmp_committed_rot13 "$TEST_ROOT/test.o" test-delay10.a &&
		test_cmp_committed_rot13 "$TEST_ROOT/test.o" test-delay11.a &&
		test_cmp_committed_rot13 "$TEST_ROOT/test.o" test-delay20.a &&
		test_cmp_committed_rot13 "$TEST_ROOT/test.o" test-delay10.b
	)
'

test_expect_success PERL 'missing file in delayed checkout' '
	test_config_global filter.bug.process "rot13-filter.pl bug.log clean smudge delay" &&
	test_config_global filter.bug.required true &&

	rm -rf repo &&
	mkdir repo &&
	(
		cd repo &&
		git init &&
		echo "*.a filter=bug" >.gitattributes &&
		cp "$TEST_ROOT/test.o" missing-delay.a &&
		git add . &&
		git commit -m "test commit"
	) &&

	rm -rf repo-cloned &&
	test_must_fail git clone repo repo-cloned 2>git-stderr.log &&
	grep "error: .missing-delay\.a. was not filtered properly" git-stderr.log
'

test_expect_success PERL 'invalid file in delayed checkout' '
	test_config_global filter.bug.process "rot13-filter.pl bug.log clean smudge delay" &&
	test_config_global filter.bug.required true &&

	rm -rf repo &&
	mkdir repo &&
	(
		cd repo &&
		git init &&
		echo "*.a filter=bug" >.gitattributes &&
		cp "$TEST_ROOT/test.o" invalid-delay.a &&
		cp "$TEST_ROOT/test.o" unfiltered &&
		git add . &&
		git commit -m "test commit"
	) &&

	rm -rf repo-cloned &&
	test_must_fail git clone repo repo-cloned 2>git-stderr.log &&
	grep "error: external filter .* signaled that .unfiltered. is now available although it has not been delayed earlier" git-stderr.log
'

test_done
