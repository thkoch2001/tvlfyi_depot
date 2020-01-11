#!/bin/sh
# This requires git-manpages and/or git-htmldocs repositories

repository=${1?repository}
destdir=${2?destination}
GIT_MAN_REF=${3?master}

GIT_DIR=
for d in "$repository/.git" "$repository"
do
	if GIT_DIR="$d" git rev-parse "$GIT_MAN_REF" >/dev/null 2>&1
	then
		GIT_DIR="$d"
		export GIT_DIR
		break
	fi
done

if test -z "$GIT_DIR"
then
	echo >&2 "Neither $repository nor $repository/.git is a repository"
	exit 1
fi

GIT_WORK_TREE=$(pwd)
GIT_INDEX_FILE=$(pwd)/.quick-doc.$$
export GIT_INDEX_FILE GIT_WORK_TREE
rm -f "$GIT_INDEX_FILE"
trap 'rm -f "$GIT_INDEX_FILE"' 0

git read-tree "$GIT_MAN_REF"
git checkout-index -a -f --prefix="$destdir"/

if test -n "$GZ"
then
	git ls-tree -r --name-only "$GIT_MAN_REF" |
	xargs printf "$destdir/%s\n" |
	xargs gzip -f
fi
rm -f "$GIT_INDEX_FILE"
