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


# wgcheckout combined with a fuzzy search
function wgfcheckout {
  branchname=$(trim $(git branch | fzf))

  [ ! -z "$branchname" ] && wgcheckout "$branchname" || return
}


# combine fetch and rebase (git frebase)
function wgfreebase {
    if [ -z $1 ]; then
        branchname="$(git symbolic-ref HEAD 2> /dev/null | cut -f3 -d'/')"
    else
        branchname="$1"
    fi

    git fetch origin "$branchname" && git rebase origin/"$branchname"
}


# push to current branch
function wgpush {
    if [ -z $1 ]; then
        branchname="$(git symbolic-ref HEAD 2> /dev/null | cut -f3 -d'/')"
    else
        branchname="$1"
    fi

    git push origin $branchname
}

