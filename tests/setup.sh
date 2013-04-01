# This file should be sourced by all test-scripts
#
# Main functions:
#   prepare_tests(description) - setup for testing, i.e. create repos+config
#   run_test(description, script) - run one test, i.e. eval script
#
# Helper functions
#   cgit_query(querystring) - call cgit with the specified querystring
#   cgit_url(url) - call cgit with the specified virtual url
#
# Example script:
#
# . setup.sh
# prepare_tests "html validation"
# run_test 'repo index' 'cgit_url "/" | tidy -e'
# run_test 'repo summary' 'cgit_url "/foo" | tidy -e'

: ${TEST_DIRECTORY=$(pwd)/../git/t}
TEST_NO_CREATE_REPO=YesPlease
. "$TEST_DIRECTORY"/test-lib.sh

# Prepend the directory containing cgit to PATH.
PATH="$(pwd)/../..:$PATH"

mkrepo() {
	name=$1
	count=$2
	test_create_repo "$name"
	(
		cd "$name"
		n=1
		while test $n -le $count
		do
			echo $n >file-$n
			git add file-$n
			git commit -m "commit $n"
			n=$(expr $n + 1)
		done
		if test "$3" = "testplus"
		then
			echo "hello" >a+b
			git add a+b
			git commit -m "add a+b"
			git branch "1+2"
		fi
	)
}

setup_repos()
{
	rm -rf cache
	mkdir -p cache
	mkrepo repos/foo 5 >/dev/null
	mkrepo repos/bar 50 >/dev/null
	mkrepo repos/foo+bar 10 testplus >/dev/null
	mkrepo "repos/with space" 2 >/dev/null
	cat >cgitrc <<EOF
virtual-root=/
cache-root=$PWD/cache

cache-size=1021
snapshots=tar.gz tar.bz zip
enable-log-filecount=1
enable-log-linecount=1
summary-log=5
summary-branches=5
summary-tags=5
clone-url=git://example.org/\$CGIT_REPO_URL.git

repo.url=foo
repo.path=$PWD/repos/foo/.git
# Do not specify a description for this repo, as it then will be assigned
# the constant value "[no description]" (which actually used to cause a
# segfault).

repo.url=bar
repo.path=$PWD/repos/bar/.git
repo.desc=the bar repo

repo.url=foo+bar
repo.path=$PWD/repos/foo+bar/.git
repo.desc=the foo+bar repo

repo.url=with space
repo.path=$PWD/repos/with space/.git
repo.desc=spaced repo
EOF
}

cgit_query()
{
	CGIT_CONFIG="$PWD/cgitrc" QUERY_STRING="$1" cgit
}

cgit_url()
{
	CGIT_CONFIG="$PWD/cgitrc" QUERY_STRING="url=$1" cgit
}

test -z "$CGIT_TEST_NO_CREATE_REPOS" && setup_repos
