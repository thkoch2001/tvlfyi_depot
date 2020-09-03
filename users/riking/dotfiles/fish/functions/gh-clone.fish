function gh-clone --description 'Clone and CD to a github repository'
    if test (count $argv) -eq 1
	set user (string split "/" -- $argv[1])[1]
	set repo (string split "/" -- $argv[1])[2]
    else
	set user $argv[1]
	set repo $argv[2]
    end

    if test -d "$HOME/go/src/github.com/$user/$repo"
	cd "$HOME/go/src/github.com/$user/$repo"
	return 0
    end
    mkdir -p "$HOME/go/src/github.com/$user"
    cd "$HOME/go/src/github.com/$user"
    git clone "git@github.com:$user/$repo.git"
    cd "$HOME/go/src/github.com/$user/$repo"
end
