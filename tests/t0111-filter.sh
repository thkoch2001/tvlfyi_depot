#!/bin/sh

test_description='Check filtered content'
. ./setup.sh

for prefix in exec lua
do
	test_expect_success "generate filter-$prefix/tree/a%2bb" "
		cgit_url 'filter-$prefix/tree/a%2bb' >tmp
	"

	test_expect_success "check whether the $prefix source filter works" '
		grep "<code>a+b HELLO$" tmp
	'

	test_expect_success "generate filter-$prefix/about/" "
		cgit_url 'filter-$prefix/about/' >tmp
	"

	test_expect_success "check whether the $prefix about filter works" '
		grep "<div id='"'"'summary'"'"'>a+b HELLO$" tmp
	'

	test_expect_success "generate filter-$prefix/commit/" "
		cgit_url 'filter-$prefix/commit/' >tmp
	"

	test_expect_success "check whether the $prefix commit filter works" '
		grep "<div class='"'"'commit-subject'"'"'>ADD A+B" tmp
	'

	test_expect_success "check whether the $prefix email filter works for authors" '
		grep "A U THOR &LT;AUTHOR@EXAMPLE.COM&GT;" tmp
	'

	test_expect_success "check whether the $prefix email filter works for committers" '
		grep "C O MITTER &LT;COMMITTER@EXAMPLE.COM&GT;" tmp
	'
done

test_done
