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
