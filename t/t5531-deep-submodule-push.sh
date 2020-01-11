#!/bin/sh

test_description='test push with submodules'

. ./test-lib.sh

test_expect_success setup '
	mkdir pub.git &&
	GIT_DIR=pub.git git init --bare &&
	GIT_DIR=pub.git git config receive.fsckobjects true &&
	mkdir work &&
	(
		cd work &&
		git init &&
		git config push.default matching &&
		mkdir -p gar/bage &&
		(
			cd gar/bage &&
			git init &&
			git config push.default matching &&
			>junk &&
			git add junk &&
			git commit -m "Initial junk"
		) &&
		git add gar/bage &&
		git commit -m "Initial superproject"
	)
'

test_expect_success 'push works with recorded gitlink' '
	(
		cd work &&
		git push ../pub.git master
	)
'

test_expect_success 'push if submodule has no remote' '
	(
		cd work/gar/bage &&
		>junk2 &&
		git add junk2 &&
		git commit -m "Second junk"
	) &&
	(
		cd work &&
		git add gar/bage &&
		git commit -m "Second commit for gar/bage" &&
		git push --recurse-submodules=check ../pub.git master
	)
'

test_expect_success 'push fails if submodule commit not on remote' '
	(
		cd work/gar &&
		git clone --bare bage ../../submodule.git &&
		cd bage &&
		git remote add origin ../../../submodule.git &&
		git fetch &&
		>junk3 &&
		git add junk3 &&
		git commit -m "Third junk"
	) &&
	(
		cd work &&
		git add gar/bage &&
		git commit -m "Third commit for gar/bage" &&
		# the push should fail with --recurse-submodules=check
		# on the command line...
		test_must_fail git push --recurse-submodules=check ../pub.git master &&

		# ...or if specified in the configuration..
		test_must_fail git -c push.recurseSubmodules=check push ../pub.git master
	)
'

test_expect_success 'push succeeds after commit was pushed to remote' '
	(
		cd work/gar/bage &&
		git push origin master
	) &&
	(
		cd work &&
		git push --recurse-submodules=check ../pub.git master
	)
'

test_expect_success 'push succeeds if submodule commit not on remote but using on-demand on command line' '
	(
		cd work/gar/bage &&
		>recurse-on-demand-on-command-line &&
		git add recurse-on-demand-on-command-line &&
		git commit -m "Recurse on-demand on command line junk"
	) &&
	(
		cd work &&
		git add gar/bage &&
		git commit -m "Recurse on-demand on command line for gar/bage" &&
		git push --recurse-submodules=on-demand ../pub.git master &&
		# Check that the supermodule commit got there
		git fetch ../pub.git &&
		git diff --quiet FETCH_HEAD master &&
		# Check that the submodule commit got there too
		cd gar/bage &&
		git diff --quiet origin/master master
	)
'

test_expect_success 'push succeeds if submodule commit not on remote but using on-demand from config' '
	(
		cd work/gar/bage &&
		>recurse-on-demand-from-config &&
		git add recurse-on-demand-from-config &&
		git commit -m "Recurse on-demand from config junk"
	) &&
	(
		cd work &&
		git add gar/bage &&
		git commit -m "Recurse on-demand from config for gar/bage" &&
		git -c push.recurseSubmodules=on-demand push ../pub.git master &&
		# Check that the supermodule commit got there
		git fetch ../pub.git &&
		git diff --quiet FETCH_HEAD master &&
		# Check that the submodule commit got there too
		cd gar/bage &&
		git diff --quiet origin/master master
	)
'

test_expect_success 'push succeeds if submodule commit not on remote but using auto-on-demand via submodule.recurse config' '
	(
		cd work/gar/bage &&
		>recurse-on-demand-from-submodule-recurse-config &&
		git add recurse-on-demand-from-submodule-recurse-config &&
		git commit -m "Recurse submodule.recurse from config junk"
	) &&
	(
		cd work &&
		git add gar/bage &&
		git commit -m "Recurse submodule.recurse from config for gar/bage" &&
		git -c submodule.recurse push ../pub.git master &&
		# Check that the supermodule commit got there
		git fetch ../pub.git &&
		git diff --quiet FETCH_HEAD master &&
		# Check that the submodule commit got there too
		cd gar/bage &&
		git diff --quiet origin/master master
	)
'

test_expect_success 'push recurse-submodules on command line overrides config' '
	(
		cd work/gar/bage &&
		>recurse-check-on-command-line-overriding-config &&
		git add recurse-check-on-command-line-overriding-config &&
		git commit -m "Recurse on command-line overriding config junk"
	) &&
	(
		cd work &&
		git add gar/bage &&
		git commit -m "Recurse on command-line overriding config for gar/bage" &&

		# Ensure that we can override on-demand in the config
		# to just check submodules
		test_must_fail git -c push.recurseSubmodules=on-demand push --recurse-submodules=check ../pub.git master &&
		# Check that the supermodule commit did not get there
		git fetch ../pub.git &&
		git diff --quiet FETCH_HEAD master^ &&
		# Check that the submodule commit did not get there
		(cd gar/bage && git diff --quiet origin/master master^) &&

		# Ensure that we can override check in the config to
		# disable submodule recursion entirely
		(cd gar/bage && git diff --quiet origin/master master^) &&
		git -c push.recurseSubmodules=on-demand push --recurse-submodules=no ../pub.git master &&
		git fetch ../pub.git &&
		git diff --quiet FETCH_HEAD master &&
		(cd gar/bage && git diff --quiet origin/master master^) &&

		# Ensure that we can override check in the config to
		# disable submodule recursion entirely (alternative form)
		git -c push.recurseSubmodules=on-demand push --no-recurse-submodules ../pub.git master &&
		git fetch ../pub.git &&
		git diff --quiet FETCH_HEAD master &&
		(cd gar/bage && git diff --quiet origin/master master^) &&

		# Ensure that we can override check in the config to
		# push the submodule too
		git -c push.recurseSubmodules=check push --recurse-submodules=on-demand ../pub.git master &&
		git fetch ../pub.git &&
		git diff --quiet FETCH_HEAD master &&
		(cd gar/bage && git diff --quiet origin/master master)
	)
'

test_expect_success 'push recurse-submodules last one wins on command line' '
	(
		cd work/gar/bage &&
		>recurse-check-on-command-line-overriding-earlier-command-line &&
		git add recurse-check-on-command-line-overriding-earlier-command-line &&
		git commit -m "Recurse on command-line overridiing earlier command-line junk"
	) &&
	(
		cd work &&
		git add gar/bage &&
		git commit -m "Recurse on command-line overriding earlier command-line for gar/bage" &&

		# should result in "check"
		test_must_fail git push --recurse-submodules=on-demand --recurse-submodules=check ../pub.git master &&
		# Check that the supermodule commit did not get there
		git fetch ../pub.git &&
		git diff --quiet FETCH_HEAD master^ &&
		# Check that the submodule commit did not get there
		(cd gar/bage && git diff --quiet origin/master master^) &&

		# should result in "no"
		git push --recurse-submodules=on-demand --recurse-submodules=no ../pub.git master &&
		# Check that the supermodule commit did get there
		git fetch ../pub.git &&
		git diff --quiet FETCH_HEAD master &&
		# Check that the submodule commit did not get there
		(cd gar/bage && git diff --quiet origin/master master^) &&

		# should result in "no"
		git push --recurse-submodules=on-demand --no-recurse-submodules ../pub.git master &&
		# Check that the submodule commit did not get there
		(cd gar/bage && git diff --quiet origin/master master^) &&

		# But the options in the other order should push the submodule
		git push --recurse-submodules=check --recurse-submodules=on-demand ../pub.git master &&
		# Check that the submodule commit did get there
		git fetch ../pub.git &&
		(cd gar/bage && git diff --quiet origin/master master)
	)
'

test_expect_success 'push succeeds if submodule commit not on remote using on-demand from cmdline overriding config' '
	(
		cd work/gar/bage &&
		>recurse-on-demand-on-command-line-overriding-config &&
		git add recurse-on-demand-on-command-line-overriding-config &&
		git commit -m "Recurse on-demand on command-line overriding config junk"
	) &&
	(
		cd work &&
		git add gar/bage &&
		git commit -m "Recurse on-demand on command-line overriding config for gar/bage" &&
		git -c push.recurseSubmodules=check push --recurse-submodules=on-demand ../pub.git master &&
		# Check that the supermodule commit got there
		git fetch ../pub.git &&
		git diff --quiet FETCH_HEAD master &&
		# Check that the submodule commit got there
		cd gar/bage &&
		git diff --quiet origin/master master
	)
'

test_expect_success 'push succeeds if submodule commit disabling recursion from cmdline overriding config' '
	(
		cd work/gar/bage &&
		>recurse-disable-on-command-line-overriding-config &&
		git add recurse-disable-on-command-line-overriding-config &&
		git commit -m "Recurse disable on command-line overriding config junk"
	) &&
	(
		cd work &&
		git add gar/bage &&
		git commit -m "Recurse disable on command-line overriding config for gar/bage" &&
		git -c push.recurseSubmodules=check push --recurse-submodules=no ../pub.git master &&
		# Check that the supermodule commit got there
		git fetch ../pub.git &&
		git diff --quiet FETCH_HEAD master &&
		# But that the submodule commit did not
		( cd gar/bage && git diff --quiet origin/master master^ ) &&
		# Now push it to avoid confusing future tests
		git push --recurse-submodules=on-demand ../pub.git master
	)
'

test_expect_success 'push succeeds if submodule commit disabling recursion from cmdline (alternative form) overriding config' '
	(
		cd work/gar/bage &&
		>recurse-disable-on-command-line-alt-overriding-config &&
		git add recurse-disable-on-command-line-alt-overriding-config &&
		git commit -m "Recurse disable on command-line alternative overriding config junk"
	) &&
	(
		cd work &&
		git add gar/bage &&
		git commit -m "Recurse disable on command-line alternative overriding config for gar/bage" &&
		git -c push.recurseSubmodules=check push --no-recurse-submodules ../pub.git master &&
		# Check that the supermodule commit got there
		git fetch ../pub.git &&
		git diff --quiet FETCH_HEAD master &&
		# But that the submodule commit did not
		( cd gar/bage && git diff --quiet origin/master master^ ) &&
		# Now push it to avoid confusing future tests
		git push --recurse-submodules=on-demand ../pub.git master
	)
'

test_expect_success 'submodule entry pointing at a tag is error' '
	git -C work/gar/bage tag -a test1 -m "tag" &&
	tag=$(git -C work/gar/bage rev-parse test1^{tag}) &&
	git -C work update-index --cacheinfo 160000 "$tag" gar/bage &&
	git -C work commit -m "bad commit" &&
	test_when_finished "git -C work reset --hard HEAD^" &&
	test_must_fail git -C work push --recurse-submodules=on-demand ../pub.git master 2>err &&
	test_i18ngrep "is a tag, not a commit" err
'

test_expect_success 'push fails if recurse submodules option passed as yes' '
	(
		cd work/gar/bage &&
		>recurse-push-fails-if-recurse-submodules-passed-as-yes &&
		git add recurse-push-fails-if-recurse-submodules-passed-as-yes &&
		git commit -m "Recurse push fails if recurse submodules option passed as yes"
	) &&
	(
		cd work &&
		git add gar/bage &&
		git commit -m "Recurse push fails if recurse submodules option passed as yes for gar/bage" &&
		test_must_fail git push --recurse-submodules=yes ../pub.git master &&
		test_must_fail git -c push.recurseSubmodules=yes push ../pub.git master &&
		git push --recurse-submodules=on-demand ../pub.git master
	)
'

test_expect_success 'push fails when commit on multiple branches if one branch has no remote' '
	(
		cd work/gar/bage &&
		>junk4 &&
		git add junk4 &&
		git commit -m "Fourth junk"
	) &&
	(
		cd work &&
		git branch branch2 &&
		git add gar/bage &&
		git commit -m "Fourth commit for gar/bage" &&
		git checkout branch2 &&
		(
			cd gar/bage &&
			git checkout HEAD~1
		) &&
		>junk1 &&
		git add junk1 &&
		git commit -m "First junk" &&
		test_must_fail git push --recurse-submodules=check ../pub.git
	)
'

test_expect_success 'push succeeds if submodule has no remote and is on the first superproject commit' '
	git init --bare a &&
	git clone a a1 &&
	(
		cd a1 &&
		git init b &&
		(
			cd b &&
			>junk &&
			git add junk &&
			git commit -m "initial"
		) &&
		git add b &&
		git commit -m "added submodule" &&
		git push --recurse-submodules=check origin master
	)
'

test_expect_success 'push unpushed submodules when not needed' '
	(
		cd work &&
		(
			cd gar/bage &&
			git checkout master &&
			>junk5 &&
			git add junk5 &&
			git commit -m "Fifth junk" &&
			git push &&
			git rev-parse origin/master >../../../expected
		) &&
		git checkout master &&
		git add gar/bage &&
		git commit -m "Fifth commit for gar/bage" &&
		git push --recurse-submodules=on-demand ../pub.git master
	) &&
	(
		cd submodule.git &&
		git rev-parse master >../actual
	) &&
	test_cmp expected actual
'

test_expect_success 'push unpushed submodules when not needed 2' '
	(
		cd submodule.git &&
		git rev-parse master >../expected
	) &&
	(
		cd work &&
		(
			cd gar/bage &&
			>junk6 &&
			git add junk6 &&
			git commit -m "Sixth junk"
		) &&
		>junk2 &&
		git add junk2 &&
		git commit -m "Second junk for work" &&
		git push --recurse-submodules=on-demand ../pub.git master
	) &&
	(
		cd submodule.git &&
		git rev-parse master >../actual
	) &&
	test_cmp expected actual
'

test_expect_success 'push unpushed submodules recursively' '
	(
		cd work &&
		(
			cd gar/bage &&
			git checkout master &&
			> junk7 &&
			git add junk7 &&
			git commit -m "Seventh junk" &&
			git rev-parse master >../../../expected
		) &&
		git checkout master &&
		git add gar/bage &&
		git commit -m "Seventh commit for gar/bage" &&
		git push --recurse-submodules=on-demand ../pub.git master
	) &&
	(
		cd submodule.git &&
		git rev-parse master >../actual
	) &&
	test_cmp expected actual
'

test_expect_success 'push unpushable submodule recursively fails' '
	(
		cd work &&
		(
			cd gar/bage &&
			git rev-parse origin/master >../../../expected &&
			git checkout master~0 &&
			> junk8 &&
			git add junk8 &&
			git commit -m "Eighth junk"
		) &&
		git add gar/bage &&
		git commit -m "Eighth commit for gar/bage" &&
		test_must_fail git push --recurse-submodules=on-demand ../pub.git master
	) &&
	(
		cd submodule.git &&
		git rev-parse master >../actual
	) &&
	test_when_finished git -C work reset --hard master^ &&
	test_cmp expected actual
'

test_expect_success 'push --dry-run does not recursively update submodules' '
	(
		cd work/gar/bage &&
		git checkout master &&
		git rev-parse master >../../../expected_submodule &&
		> junk9 &&
		git add junk9 &&
		git commit -m "Ninth junk" &&

		# Go up to 'work' directory
		cd ../.. &&
		git checkout master &&
		git rev-parse master >../expected_pub &&
		git add gar/bage &&
		git commit -m "Ninth commit for gar/bage" &&
		git push --dry-run --recurse-submodules=on-demand ../pub.git master
	) &&
	git -C submodule.git rev-parse master >actual_submodule &&
	git -C pub.git rev-parse master >actual_pub &&
	test_cmp expected_pub actual_pub &&
	test_cmp expected_submodule actual_submodule
'

test_expect_success 'push --dry-run does not recursively update submodules' '
	git -C work push --dry-run --recurse-submodules=only ../pub.git master &&

	git -C submodule.git rev-parse master >actual_submodule &&
	git -C pub.git rev-parse master >actual_pub &&
	test_cmp expected_pub actual_pub &&
	test_cmp expected_submodule actual_submodule
'

test_expect_success 'push only unpushed submodules recursively' '
	git -C work/gar/bage rev-parse master >expected_submodule &&
	git -C pub.git rev-parse master >expected_pub &&

	git -C work push --recurse-submodules=only ../pub.git master &&

	git -C submodule.git rev-parse master >actual_submodule &&
	git -C pub.git rev-parse master >actual_pub &&
	test_cmp expected_submodule actual_submodule &&
	test_cmp expected_pub actual_pub
'

test_expect_success 'push propagating the remotes name to a submodule' '
	git -C work remote add origin ../pub.git &&
	git -C work remote add pub ../pub.git &&

	> work/gar/bage/junk10 &&
	git -C work/gar/bage add junk10 &&
	git -C work/gar/bage commit -m "Tenth junk" &&
	git -C work add gar/bage &&
	git -C work commit -m "Tenth junk added to gar/bage" &&

	# Fails when submodule does not have a matching remote
	test_must_fail git -C work push --recurse-submodules=on-demand pub master &&
	# Succeeds when submodules has matching remote and refspec
	git -C work push --recurse-submodules=on-demand origin master &&

	git -C submodule.git rev-parse master >actual_submodule &&
	git -C pub.git rev-parse master >actual_pub &&
	git -C work/gar/bage rev-parse master >expected_submodule &&
	git -C work rev-parse master >expected_pub &&
	test_cmp expected_submodule actual_submodule &&
	test_cmp expected_pub actual_pub
'

test_expect_success 'push propagating refspec to a submodule' '
	> work/gar/bage/junk11 &&
	git -C work/gar/bage add junk11 &&
	git -C work/gar/bage commit -m "Eleventh junk" &&

	git -C work checkout branch2 &&
	git -C work add gar/bage &&
	git -C work commit -m "updating gar/bage in branch2" &&

	# Fails when submodule does not have a matching branch
	test_must_fail git -C work push --recurse-submodules=on-demand origin branch2 &&
	# Fails when refspec includes an object id
	test_must_fail git -C work push --recurse-submodules=on-demand origin \
		"$(git -C work rev-parse branch2):refs/heads/branch2" &&
	# Fails when refspec includes HEAD and parent and submodule do not
	# have the same named branch checked out
	test_must_fail git -C work push --recurse-submodules=on-demand origin \
		HEAD:refs/heads/branch2 &&

	git -C work/gar/bage branch branch2 master &&
	git -C work push --recurse-submodules=on-demand origin branch2 &&

	git -C submodule.git rev-parse branch2 >actual_submodule &&
	git -C pub.git rev-parse branch2 >actual_pub &&
	git -C work/gar/bage rev-parse branch2 >expected_submodule &&
	git -C work rev-parse branch2 >expected_pub &&
	test_cmp expected_submodule actual_submodule &&
	test_cmp expected_pub actual_pub
'

test_expect_success 'push propagating HEAD refspec to a submodule' '
	git -C work/gar/bage checkout branch2 &&
	> work/gar/bage/junk12 &&
	git -C work/gar/bage add junk12 &&
	git -C work/gar/bage commit -m "Twelfth junk" &&

	git -C work checkout branch2 &&
	git -C work add gar/bage &&
	git -C work commit -m "updating gar/bage in branch2" &&

	# Passes since the superproject and submodules HEAD are both on branch2
	git -C work push --recurse-submodules=on-demand origin \
		HEAD:refs/heads/branch2 &&

	git -C submodule.git rev-parse branch2 >actual_submodule &&
	git -C pub.git rev-parse branch2 >actual_pub &&
	git -C work/gar/bage rev-parse branch2 >expected_submodule &&
	git -C work rev-parse branch2 >expected_pub &&
	test_cmp expected_submodule actual_submodule &&
	test_cmp expected_pub actual_pub
'

test_done
