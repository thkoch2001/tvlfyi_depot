#!/bin/sh

test_description='git cat-file textconv support'
. ./test-lib.sh

cat >helper <<'EOF'
#!/bin/sh
grep -q '^bin: ' "$1" || { echo "E: $1 is not \"binary\" file" 1>&2; exit 1; }
sed 's/^bin: /converted: /' "$1"
EOF
chmod +x helper

test_expect_success 'setup ' '
	echo "bin: test" >one.bin &&
	test_ln_s_add one.bin symlink.bin &&
	git add . &&
	GIT_AUTHOR_NAME=Number1 git commit -a -m First --date="2010-01-01 18:00:00" &&
	echo "bin: test version 2" >one.bin &&
	GIT_AUTHOR_NAME=Number2 git commit -a -m Second --date="2010-01-01 20:00:00"
'

cat >expected <<EOF
bin: test version 2
EOF

test_expect_success 'no filter specified' '
	git cat-file --textconv :one.bin >result &&
	test_cmp expected result
'

test_expect_success 'setup textconv filters' '
	echo "*.bin diff=test" >.gitattributes &&
	git config diff.test.textconv ./helper &&
	git config diff.test.cachetextconv false
'

test_expect_success 'cat-file without --textconv' '
	git cat-file blob :one.bin >result &&
	test_cmp expected result
'

cat >expected <<EOF
bin: test
EOF

test_expect_success 'cat-file without --textconv on previous commit' '
	git cat-file -p HEAD^:one.bin >result &&
	test_cmp expected result
'

cat >expected <<EOF
converted: test version 2
EOF

test_expect_success 'cat-file --textconv on last commit' '
	git cat-file --textconv :one.bin >result &&
	test_cmp expected result
'

cat >expected <<EOF
converted: test
EOF

test_expect_success 'cat-file --textconv on previous commit' '
	git cat-file --textconv HEAD^:one.bin >result &&
	test_cmp expected result
'

test_expect_success 'cat-file without --textconv (symlink)' '
	printf "%s" "one.bin" >expected &&
	git cat-file blob :symlink.bin >result &&
	test_cmp expected result
'


test_expect_success 'cat-file --textconv on index (symlink)' '
	git cat-file --textconv :symlink.bin >result &&
	test_cmp expected result
'

test_expect_success 'cat-file --textconv on HEAD (symlink)' '
	git cat-file --textconv HEAD:symlink.bin >result &&
	test_cmp expected result
'

test_done
