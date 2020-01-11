#!/bin/sh

# Usage: Run 'contrib/coverage-diff.sh <version1> <version2>' from source-root
# after running
#
#     make coverage-test
#     make coverage-report
#
# while checked out at <version2>. This script combines the *.gcov files
# generated by the 'make' commands above with 'git diff <version1> <version2>'
# to report new lines that are not covered by the test suite.

V1=$1
V2=$2

diff_lines () {
	perl -e '
		my $line_num;
		while (<>) {
			# Hunk header?  Grab the beginning in postimage.
			if (/^@@ -\d+(?:,\d+)? \+(\d+)(?:,\d+)? @@/) {
				$line_num = $1;
				next;
			}

			# Have we seen a hunk?  Ignore "diff --git" etc.
			next unless defined $line_num;

			# Deleted line? Ignore.
			if (/^-/) {
				next;
			}

			# Show only the line number of added lines.
			if (/^\+/) {
				print "$line_num\n";
			}
			# Either common context or added line appear in
			# the postimage.  Count it.
			$line_num++;
		}
	'
}

files=$(git diff --name-only "$V1" "$V2" -- \*.c)

# create empty file
>coverage-data.txt

for file in $files
do
	git diff "$V1" "$V2" -- "$file" |
	diff_lines |
	sort >new_lines.txt

	if ! test -s new_lines.txt
	then
		continue
	fi

	hash_file=$(echo $file | sed "s/\//\#/")

	if ! test -s "$hash_file.gcov"
	then
		continue
	fi

	sed -ne '/#####:/{
			s/    #####://
			s/:.*//
			s/ //g
			p
		}' "$hash_file.gcov" |
	sort >uncovered_lines.txt

	comm -12 uncovered_lines.txt new_lines.txt |
	sed -e 's/$/\)/' |
	sed -e 's/^/ /' >uncovered_new_lines.txt

	grep -q '[^[:space:]]' <uncovered_new_lines.txt &&
	echo $file >>coverage-data.txt &&
	git blame -s "$V2" -- "$file" |
	sed 's/\t//g' |
	grep -f uncovered_new_lines.txt >>coverage-data.txt &&
	echo >>coverage-data.txt

	rm -f new_lines.txt uncovered_lines.txt uncovered_new_lines.txt
done

cat coverage-data.txt

echo "Commits introducing uncovered code:"

commit_list=$(cat coverage-data.txt |
	grep -E '^[0-9a-f]{7,} ' |
	awk '{print $1;}' |
	sort |
	uniq)

(
	for commit in $commit_list
	do
		git log --no-decorate --pretty=format:'%an      %h: %s' -1 $commit
		echo
	done
) | sort

rm coverage-data.txt
