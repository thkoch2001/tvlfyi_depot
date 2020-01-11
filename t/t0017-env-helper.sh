#!/bin/sh

test_description='test env--helper'

. ./test-lib.sh


test_expect_success 'env--helper usage' '
	test_must_fail git env--helper &&
	test_must_fail git env--helper --type=bool &&
	test_must_fail git env--helper --type=ulong &&
	test_must_fail git env--helper --type=bool &&
	test_must_fail git env--helper --type=bool --default &&
	test_must_fail git env--helper --type=bool --default= &&
	test_must_fail git env--helper --defaultxyz
'

test_expect_success 'env--helper bad default values' '
	test_must_fail git env--helper --type=bool --default=1xyz MISSING &&
	test_must_fail git env--helper --type=ulong --default=1xyz MISSING
'

test_expect_success 'env--helper --type=bool' '
	# Test various --default bool values
	echo true >expected &&
	git env--helper --type=bool --default=1 MISSING >actual &&
	test_cmp expected actual &&
	git env--helper --type=bool --default=yes MISSING >actual &&
	test_cmp expected actual &&
	git env--helper --type=bool --default=true MISSING >actual &&
	test_cmp expected actual &&
	echo false >expected &&
	test_must_fail git env--helper --type=bool --default=0 MISSING >actual &&
	test_cmp expected actual &&
	test_must_fail git env--helper --type=bool --default=no MISSING >actual &&
	test_cmp expected actual &&
	test_must_fail git env--helper --type=bool --default=false MISSING >actual &&
	test_cmp expected actual &&

	# No output with --exit-code
	git env--helper --type=bool --default=true --exit-code MISSING >actual.out 2>actual.err &&
	test_must_be_empty actual.out &&
	test_must_be_empty actual.err &&
	test_must_fail git env--helper --type=bool --default=false --exit-code MISSING >actual.out 2>actual.err &&
	test_must_be_empty actual.out &&
	test_must_be_empty actual.err &&

	# Existing variable
	EXISTS=true git env--helper --type=bool --default=false --exit-code EXISTS >actual.out 2>actual.err &&
	test_must_be_empty actual.out &&
	test_must_be_empty actual.err &&
	test_must_fail \
		env EXISTS=false \
		git env--helper --type=bool --default=true --exit-code EXISTS >actual.out 2>actual.err &&
	test_must_be_empty actual.out &&
	test_must_be_empty actual.err
'

test_expect_success 'env--helper --type=ulong' '
	echo 1234567890 >expected &&
	git env--helper --type=ulong --default=1234567890 MISSING >actual.out 2>actual.err &&
	test_cmp expected actual.out &&
	test_must_be_empty actual.err &&

	echo 0 >expected &&
	test_must_fail git env--helper --type=ulong --default=0 MISSING >actual &&
	test_cmp expected actual &&

	git env--helper --type=ulong --default=1234567890 --exit-code MISSING >actual.out 2>actual.err &&
	test_must_be_empty actual.out &&
	test_must_be_empty actual.err &&

	EXISTS=1234567890 git env--helper --type=ulong --default=0 EXISTS --exit-code >actual.out 2>actual.err &&
	test_must_be_empty actual.out &&
	test_must_be_empty actual.err &&

	echo 1234567890 >expected &&
	EXISTS=1234567890 git env--helper --type=ulong --default=0 EXISTS >actual.out 2>actual.err &&
	test_cmp expected actual.out &&
	test_must_be_empty actual.err
'

test_expect_success 'env--helper reads config thanks to trace2' '
	mkdir home &&
	git config -f home/.gitconfig include.path cycle &&
	git config -f home/cycle include.path .gitconfig &&

	test_must_fail \
		env HOME="$(pwd)/home" GIT_TEST_GETTEXT_POISON=false \
		git config -l 2>err &&
	grep "exceeded maximum include depth" err &&

	test_must_fail \
		env HOME="$(pwd)/home" GIT_TEST_GETTEXT_POISON=true \
		git -C cycle env--helper --type=bool --default=0 --exit-code GIT_TEST_GETTEXT_POISON 2>err &&
	grep "# GETTEXT POISON #" err
'

test_done
