# output current branch to STDOUT
function wgbranch {
  cat ./.git/HEAD | perl -p -e 's/^ref:\srefs\/heads\/(.+)$/\1/g'
}


function git-discard {
  option=$1

  if [[ $option == '' ]]; then
      echo "Please supply option: --staged, --unstaged, --untracked, or --all"
  fi

  if [[ $option == '--all' ]]; then
      git-discard --staged && git-discard --unstaged && git-discard --untracked
  fi

  if [[ $option == '--staged' ]]; then
      staged_files=$(git --no-pager diff --name-only --staged)

      echo -n "Discarding staged..." &&
      git reset HEAD $staged_files >/dev/null &&
      echo "done."
  fi

  if [[ $option == '--unstaged' ]]; then
      unstaged_files=$(git --no-pager diff --name-only)

      echo -n "Discarding unstaged..." &&
      git checkout -- $unstaged_files >/dev/null
      echo "done."
  fi

  if [[ $option == '--untracked' ]]; then
      untracked_files=$(git ls-files --others --exclude-standard)

      echo -n "Discarding untracked..."
      for file in $untracked_files; do
        if [ -f $file ]; then
            rm $file
        else
          rm -rf $file
        fi
      done
      echo "done."
  fi
}


function git-list {
}


# Outputs staged, unstaged, untracked files
# Similar to `git status` output but without the cruft
function wg-git-changed-files {
  tracked_staged=$(wg-diff-tracked-staged)
  tracked_unstaged=$(wg-diff-tracked-unstaged)
  untracked_unstaged=$(wg-diff-untracked-unstaged)

  echo "${tracked_staged}\n${tracked_unstaged}\n${untracked_unstaged}"
}

function wg-diff-tracked-staged {
  git --no-pager diff --name-only --staged
}

function wg-diff-tracked-unstaged {
  git --no-pager diff --name-only
}

function wg-diff-untracked-unstaged {
  git ls-files --others --exclude-standard
}


# git status "plumbing" version
# Useful for piping into grep -> xargs git add
function wgst {
  git status -s | awk '{ print $2 }'
}


# git add by file regex pattern
function wgadd {
  pattern="$2"

  wgst | grep "${pattern}" | xargs git add
}


# compare file with another branch
function wgcompare_file {
  file_path="$1"
  compare_branch_a="master"
  compare_branch_b="$(wgbranch)"

  git diff "${compare_branch_a}:${file_path}" "${compare_branch_b}:${file_path}"
}


# output the stash ticket number to STDOUT
function wgtix {
  wgbranch | perl -p -e 's/(?:feature|bugfix|refactor)\/(\w+-\d+).+$/\1/'
}


# search for a git branch by ticket number
# useful when combined with `wgcheckout`
# e.g.
# $ wgcheckout "$(wgfind 1045)"
# checks-out feature/GDMX-1045 ...
#
# if the `TICKET_NO` cannot be found, it will return the current branch
function wgfind {
  TICKET_NO="$1"

  BRANCHNAME=$(git branch | grep "$TICKET_NO" | perl -p -e 's/^\s*//')

  if [ -z $BRANCHNAME ]; then
    BRANCHNAME="$(wgbranch)"
  fi

  echo "$BRANCHNAME"
}


# wrapper fn for "git checkout" that exports previous branch to env
function wgcheckout {
  if [ -z $1 ]; then
      branchname="develop"
  else
    branchname="$1"
  fi

  echo " -- wgcheckout -- "
  echo "Storing branch \"$(wgbranch)\" in WGPREV ..."
  export WGPREV="$(wgbranch)"
  echo "Checking out \"$branchname\" ..."
  echo
  echo " -- git checkout -- "
  git checkout "$branchname"
  echo
}


# opens the current ticket-branch in web browser
function wgjira {
  base_url="https://jira.hugeinc.com/browse"
  ticket=$(wgtix)

  open "${base_url}/${ticket}"
}


# wgcheckout combined with a fuzzy search
function wgfcheckout {
  branchname=$(trim $(git branch | fzf-tmux))

  [ ! -z "$branchname" ] && wgcheckout "$branchname" || return
}


# View an author's work within a specified date range.
function wgviewcommits {
  author=$([ -z "$1" ] && echo "William Carroll" || echo "$1")
  todays_date=$(date +'%Y-%m-%d')
  date=$([ -z "$2" ] && echo "${todays_date}" || echo "$2")

  git log --all --author="${author}" --after="${date} 00:00" \
        --before="${date} 23:59"
}
