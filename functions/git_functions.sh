# output current branch to STDOUT
function wgbranch {
  cat ./.git/HEAD | perl -p -e 's/^ref:\srefs\/heads\/(.+)$/\1/g'
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

