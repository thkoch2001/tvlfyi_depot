#!/bin/sh

test_description='git init'

. ./test-lib.sh

check_config () {
	if test -d "$1" && test -f "$1/config" && test -d "$1/refs"
	then
		: happy
	else
		echo "expected a directory $1, a file $1/config and $1/refs"
		return 1
	fi

	if test_have_prereq POSIXPERM && test -x "$1/config"
	then
		echo "$1/config is executable?"
		return 1
	fi

	bare=$(cd "$1" && git config --bool core.bare)
	worktree=$(cd "$1" && git config core.worktree) ||
	worktree=unset

	test "$bare" = "$2" && test "$worktree" = "$3" || {
		echo "expected bare=$2 worktree=$3"
		echo "     got bare=$bare worktree=$worktree"
		return 1
	}
}

test_expect_success 'plain' '
	git init plain &&
	check_config plain/.git false unset
'

test_expect_success 'plain nested in bare' '
	(
		git init --bare bare-ancestor.git &&
		cd bare-ancestor.git &&
		mkdir plain-nested &&
		cd plain-nested &&
		git init
	) &&
	check_config bare-ancestor.git/plain-nested/.git false unset
'

test_expect_success 'plain through aliased command, outside any git repo' '
	(
		HOME=$(pwd)/alias-config &&
		export HOME &&
		mkdir alias-config &&
		echo "[alias] aliasedinit = init" >alias-config/.gitconfig &&

		GIT_CEILING_DIRECTORIES=$(pwd) &&
		export GIT_CEILING_DIRECTORIES &&

		mkdir plain-aliased &&
		cd plain-aliased &&
		git aliasedinit
	) &&
	check_config plain-aliased/.git false unset
'

test_expect_success 'plain nested through aliased command' '
	(
		git init plain-ancestor-aliased &&
		cd plain-ancestor-aliased &&
		echo "[alias] aliasedinit = init" >>.git/config &&
		mkdir plain-nested &&
		cd plain-nested &&
		git aliasedinit
	) &&
	check_config plain-ancestor-aliased/plain-nested/.git false unset
'

test_expect_success 'plain nested in bare through aliased command' '
	(
		git init --bare bare-ancestor-aliased.git &&
		cd bare-ancestor-aliased.git &&
		echo "[alias] aliasedinit = init" >>config &&
		mkdir plain-nested &&
		cd plain-nested &&
		git aliasedinit
	) &&
	check_config bare-ancestor-aliased.git/plain-nested/.git false unset
'

test_expect_success 'No extra GIT_* on alias scripts' '
	write_script script <<-\EOF &&
	env |
		sed -n \
			-e "/^GIT_PREFIX=/d" \
			-e "/^GIT_TEXTDOMAINDIR=/d" \
			-e "/^GIT_TRACE2_PARENT/d" \
			-e "/^GIT_/s/=.*//p" |
		sort
	EOF
	./script >expected &&
	git config alias.script \!./script &&
	( mkdir sub && cd sub && git script >../actual ) &&
	test_cmp expected actual
'

test_expect_success 'plain with GIT_WORK_TREE' '
	mkdir plain-wt &&
	test_must_fail env GIT_WORK_TREE="$(pwd)/plain-wt" git init plain-wt
'

test_expect_success 'plain bare' '
	git --bare init plain-bare-1 &&
	check_config plain-bare-1 true unset
'

test_expect_success 'plain bare with GIT_WORK_TREE' '
	mkdir plain-bare-2 &&
	test_must_fail \
		env GIT_WORK_TREE="$(pwd)/plain-bare-2" \
		git --bare init plain-bare-2
'

test_expect_success 'GIT_DIR bare' '
	mkdir git-dir-bare.git &&
	GIT_DIR=git-dir-bare.git git init &&
	check_config git-dir-bare.git true unset
'

test_expect_success 'init --bare' '
	git init --bare init-bare.git &&
	check_config init-bare.git true unset
'

test_expect_success 'GIT_DIR non-bare' '

	(
		mkdir non-bare &&
		cd non-bare &&
		GIT_DIR=.git git init
	) &&
	check_config non-bare/.git false unset
'

test_expect_success 'GIT_DIR & GIT_WORK_TREE (1)' '

	(
		mkdir git-dir-wt-1.git &&
		GIT_WORK_TREE=$(pwd) GIT_DIR=git-dir-wt-1.git git init
	) &&
	check_config git-dir-wt-1.git false "$(pwd)"
'

test_expect_success 'GIT_DIR & GIT_WORK_TREE (2)' '
	mkdir git-dir-wt-2.git &&
	test_must_fail env \
		GIT_WORK_TREE="$(pwd)" \
		GIT_DIR=git-dir-wt-2.git \
		git --bare init
'

test_expect_success 'reinit' '

	(
		mkdir again &&
		cd again &&
		git init >out1 2>err1 &&
		git init >out2 2>err2
	) &&
	test_i18ngrep "Initialized empty" again/out1 &&
	test_i18ngrep "Reinitialized existing" again/out2 &&
	test_must_be_empty again/err1 &&
	test_must_be_empty again/err2
'

test_expect_success 'init with --template' '
	mkdir template-source &&
	echo content >template-source/file &&
	git init --template=template-source template-custom &&
	test_cmp template-source/file template-custom/.git/file
'

test_expect_success 'init with --template (blank)' '
	git init template-plain &&
	test_path_is_file template-plain/.git/info/exclude &&
	git init --template= template-blank &&
	test_path_is_missing template-blank/.git/info/exclude
'

test_expect_success 'init with init.templatedir set' '
	mkdir templatedir-source &&
	echo Content >templatedir-source/file &&
	test_config_global init.templatedir "${HOME}/templatedir-source" &&
	(
		mkdir templatedir-set &&
		cd templatedir-set &&
		sane_unset GIT_TEMPLATE_DIR &&
		NO_SET_GIT_TEMPLATE_DIR=t &&
		export NO_SET_GIT_TEMPLATE_DIR &&
		git init
	) &&
	test_cmp templatedir-source/file templatedir-set/.git/file
'

test_expect_success 'init --bare/--shared overrides system/global config' '
	test_config_global core.bare false &&
	test_config_global core.sharedRepository 0640 &&
	git init --bare --shared=0666 init-bare-shared-override &&
	check_config init-bare-shared-override true unset &&
	test x0666 = \
	x$(git config -f init-bare-shared-override/config core.sharedRepository)
'

test_expect_success 'init honors global core.sharedRepository' '
	test_config_global core.sharedRepository 0666 &&
	git init shared-honor-global &&
	test x0666 = \
	x$(git config -f shared-honor-global/.git/config core.sharedRepository)
'

test_expect_success 'init allows insanely long --template' '
	git init --template=$(printf "x%09999dx" 1) test
'

test_expect_success 'init creates a new directory' '
	rm -fr newdir &&
	git init newdir &&
	test_path_is_dir newdir/.git/refs
'

test_expect_success 'init creates a new bare directory' '
	rm -fr newdir &&
	git init --bare newdir &&
	test_path_is_dir newdir/refs
'

test_expect_success 'init recreates a directory' '
	rm -fr newdir &&
	mkdir newdir &&
	git init newdir &&
	test_path_is_dir newdir/.git/refs
'

test_expect_success 'init recreates a new bare directory' '
	rm -fr newdir &&
	mkdir newdir &&
	git init --bare newdir &&
	test_path_is_dir newdir/refs
'

test_expect_success 'init creates a new deep directory' '
	rm -fr newdir &&
	git init newdir/a/b/c &&
	test_path_is_dir newdir/a/b/c/.git/refs
'

test_expect_success POSIXPERM 'init creates a new deep directory (umask vs. shared)' '
	rm -fr newdir &&
	(
		# Leading directories should honor umask while
		# the repository itself should follow "shared"
		mkdir newdir &&
		# Remove a default ACL if possible.
		(setfacl -k newdir 2>/dev/null || true) &&
		umask 002 &&
		git init --bare --shared=0660 newdir/a/b/c &&
		test_path_is_dir newdir/a/b/c/refs &&
		ls -ld newdir/a newdir/a/b > lsab.out &&
		! grep -v "^drwxrw[sx]r-x" lsab.out &&
		ls -ld newdir/a/b/c > lsc.out &&
		! grep -v "^drwxrw[sx]---" lsc.out
	)
'

test_expect_success 'init notices EEXIST (1)' '
	rm -fr newdir &&
	>newdir &&
	test_must_fail git init newdir &&
	test_path_is_file newdir
'

test_expect_success 'init notices EEXIST (2)' '
	rm -fr newdir &&
	mkdir newdir &&
	>newdir/a &&
	test_must_fail git init newdir/a/b &&
	test_path_is_file newdir/a
'

test_expect_success POSIXPERM,SANITY 'init notices EPERM' '
	test_when_finished "chmod +w newdir" &&
	rm -fr newdir &&
	mkdir newdir &&
	chmod -w newdir &&
	test_must_fail git init newdir/a/b
'

test_expect_success 'init creates a new bare directory with global --bare' '
	rm -rf newdir &&
	git --bare init newdir &&
	test_path_is_dir newdir/refs
'

test_expect_success 'init prefers command line to GIT_DIR' '
	rm -rf newdir &&
	mkdir otherdir &&
	GIT_DIR=otherdir git --bare init newdir &&
	test_path_is_dir newdir/refs &&
	test_path_is_missing otherdir/refs
'

test_expect_success 'init with separate gitdir' '
	rm -rf newdir &&
	git init --separate-git-dir realgitdir newdir &&
	newdir_git="$(cat newdir/.git)" &&
	test_cmp_fspath "$(pwd)/realgitdir" "${newdir_git#gitdir: }" &&
	test_path_is_dir realgitdir/refs
'

test_expect_success 'explicit bare & --separate-git-dir incompatible' '
	test_must_fail git init --bare --separate-git-dir goop.git bare.git 2>err &&
	test_i18ngrep "mutually exclusive" err
'

test_expect_success 'implicit bare & --separate-git-dir incompatible' '
	test_when_finished "rm -rf bare.git" &&
	mkdir -p bare.git &&
	test_must_fail env GIT_DIR=. \
		git -C bare.git init --separate-git-dir goop.git 2>err &&
	test_i18ngrep "incompatible" err
'

test_expect_success 'bare & --separate-git-dir incompatible within worktree' '
	test_when_finished "rm -rf bare.git linkwt seprepo" &&
	test_commit gumby &&
	git clone --bare . bare.git &&
	git -C bare.git worktree add --detach ../linkwt &&
	test_must_fail git -C linkwt init --separate-git-dir seprepo 2>err &&
	test_i18ngrep "incompatible" err
'

test_lazy_prereq GETCWD_IGNORES_PERMS '
	base=GETCWD_TEST_BASE_DIR &&
	mkdir -p $base/dir &&
	chmod 100 $base ||
	BUG "cannot prepare $base"

	(cd $base/dir && /bin/pwd -P)
	status=$?

	chmod 700 $base &&
	rm -rf $base ||
	BUG "cannot clean $base"
	return $status
'

check_long_base_path () {
	# exceed initial buffer size of strbuf_getcwd()
	component=123456789abcdef &&
	test_when_finished "chmod 0700 $component; rm -rf $component" &&
	p31=$component/$component &&
	p127=$p31/$p31/$p31/$p31 &&
	mkdir -p $p127 &&
	if test $# = 1
	then
		chmod $1 $component
	fi &&
	(
		cd $p127 &&
		git init newdir
	)
}

test_expect_success 'init in long base path' '
	check_long_base_path
'

test_expect_success GETCWD_IGNORES_PERMS 'init in long restricted base path' '
	check_long_base_path 0111
'

test_expect_success 're-init on .git file' '
	( cd newdir && git init )
'

test_expect_success 're-init to update git link' '
	git -C newdir init --separate-git-dir ../surrealgitdir &&
	newdir_git="$(cat newdir/.git)" &&
	test_cmp_fspath "$(pwd)/surrealgitdir" "${newdir_git#gitdir: }" &&
	test_path_is_dir surrealgitdir/refs &&
	test_path_is_missing realgitdir/refs
'

test_expect_success 're-init to move gitdir' '
	rm -rf newdir realgitdir surrealgitdir &&
	git init newdir &&
	git -C newdir init --separate-git-dir ../realgitdir &&
	newdir_git="$(cat newdir/.git)" &&
	test_cmp_fspath "$(pwd)/realgitdir" "${newdir_git#gitdir: }" &&
	test_path_is_dir realgitdir/refs
'

test_expect_success SYMLINKS 're-init to move gitdir symlink' '
	rm -rf newdir realgitdir &&
	git init newdir &&
	(
	cd newdir &&
	mv .git here &&
	ln -s here .git &&
	git init --separate-git-dir ../realgitdir
	) &&
	echo "gitdir: $(pwd)/realgitdir" >expected &&
	test_cmp expected newdir/.git &&
	test_cmp expected newdir/here &&
	test_path_is_dir realgitdir/refs
'

sep_git_dir_worktree ()  {
	test_when_finished "rm -rf mainwt linkwt seprepo" &&
	git init mainwt &&
	test_commit -C mainwt gumby &&
	git -C mainwt worktree add --detach ../linkwt &&
	git -C "$1" init --separate-git-dir ../seprepo &&
	git -C mainwt rev-parse --git-common-dir >expect &&
	git -C linkwt rev-parse --git-common-dir >actual &&
	test_cmp expect actual
}

test_expect_success 're-init to move gitdir with linked worktrees' '
	sep_git_dir_worktree mainwt
'

test_expect_success 're-init to move gitdir within linked worktree' '
	sep_git_dir_worktree linkwt
'

test_expect_success MINGW '.git hidden' '
	rm -rf newdir &&
	(
		sane_unset GIT_DIR GIT_WORK_TREE &&
		mkdir newdir &&
		cd newdir &&
		git init &&
		test_path_is_hidden .git
	) &&
	check_config newdir/.git false unset
'

test_expect_success MINGW 'bare git dir not hidden' '
	rm -rf newdir &&
	(
		sane_unset GIT_DIR GIT_WORK_TREE GIT_CONFIG &&
		mkdir newdir &&
		cd newdir &&
		git --bare init
	) &&
	! is_hidden newdir
'

test_expect_success 'remote init from does not use config from cwd' '
	rm -rf newdir &&
	test_config core.logallrefupdates true &&
	git init newdir &&
	echo true >expect &&
	git -C newdir config --bool core.logallrefupdates >actual &&
	test_cmp expect actual
'

test_expect_success 're-init from a linked worktree' '
	git init main-worktree &&
	(
		cd main-worktree &&
		test_commit first &&
		git worktree add ../linked-worktree &&
		mv .git/info/exclude expected-exclude &&
		cp .git/config expected-config &&
		find .git/worktrees -print | sort >expected &&
		git -C ../linked-worktree init &&
		test_cmp expected-exclude .git/info/exclude &&
		test_cmp expected-config .git/config &&
		find .git/worktrees -print | sort >actual &&
		test_cmp expected actual
	)
'

test_expect_success 'init honors GIT_DEFAULT_HASH' '
	GIT_DEFAULT_HASH=sha1 git init sha1 &&
	git -C sha1 rev-parse --show-object-format >actual &&
	echo sha1 >expected &&
	test_cmp expected actual &&
	GIT_DEFAULT_HASH=sha256 git init sha256 &&
	git -C sha256 rev-parse --show-object-format >actual &&
	echo sha256 >expected &&
	test_cmp expected actual
'

test_expect_success 'init honors --object-format' '
	git init --object-format=sha1 explicit-sha1 &&
	git -C explicit-sha1 rev-parse --show-object-format >actual &&
	echo sha1 >expected &&
	test_cmp expected actual &&
	git init --object-format=sha256 explicit-sha256 &&
	git -C explicit-sha256 rev-parse --show-object-format >actual &&
	echo sha256 >expected &&
	test_cmp expected actual
'

test_expect_success 'extensions.objectFormat is not allowed with repo version 0' '
	git init --object-format=sha256 explicit-v0 &&
	git -C explicit-v0 config core.repositoryformatversion 0 &&
	test_must_fail git -C explicit-v0 rev-parse --show-object-format
'

test_expect_success 'init rejects attempts to initialize with different hash' '
	test_must_fail git -C sha1 init --object-format=sha256 &&
	test_must_fail git -C sha256 init --object-format=sha1
'

test_expect_success MINGW 'core.hidedotfiles = false' '
	git config --global core.hidedotfiles false &&
	rm -rf newdir &&
	mkdir newdir &&
	(
		sane_unset GIT_DIR GIT_WORK_TREE GIT_CONFIG &&
		git -C newdir init
	) &&
	! is_hidden newdir/.git
'

test_expect_success MINGW 'redirect std handles' '
	GIT_REDIRECT_STDOUT=output.txt git rev-parse --git-dir &&
	test .git = "$(cat output.txt)" &&
	test -z "$(GIT_REDIRECT_STDOUT=off git rev-parse --git-dir)" &&
	test_must_fail env \
		GIT_REDIRECT_STDOUT=output.txt \
		GIT_REDIRECT_STDERR="2>&1" \
		git rev-parse --git-dir --verify refs/invalid &&
	grep "^\\.git\$" output.txt &&
	grep "Needed a single revision" output.txt
'

test_expect_success '--initial-branch' '
	git init --initial-branch=hello initial-branch-option &&
	git -C initial-branch-option symbolic-ref HEAD >actual &&
	echo refs/heads/hello >expect &&
	test_cmp expect actual &&

	: re-initializing should not change the branch name &&
	git init --initial-branch=ignore initial-branch-option 2>err &&
	test_i18ngrep "ignored --initial-branch" err &&
	git -C initial-branch-option symbolic-ref HEAD >actual &&
	grep hello actual
'

test_expect_success 'overridden default initial branch name (config)' '
	test_config_global init.defaultBranch nmb &&
	git init initial-branch-config &&
	git -C initial-branch-config symbolic-ref HEAD >actual &&
	grep nmb actual
'

test_expect_success 'invalid default branch name' '
	test_config_global init.defaultBranch "with space" &&
	test_must_fail git init initial-branch-invalid 2>err &&
	test_i18ngrep "invalid branch name" err
'

test_done
