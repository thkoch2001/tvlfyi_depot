# TODO: Consider a `rm` that behaves like this as well. Would then be useful to
# support something like a "Trash" folder so that I can undo unintentional
# deletions.
function cp_dwim -d "Copy files and directories similarly."
    # TODO: Where do I put documentation like this?
    # Calls `cp -r` when a directory is specified, otherwise uses `cp`.
    # This is closer to the UX you expect in GUIs when you copy-and-paste files.
    if test -d $argv[1]
        command cp -r $argv[1..-1]
    else
        command cp $argv[1..-1]
    end
end

function mkdir_cd -d "Make a directory and `cd` into it."
    mkdir -p $argv[1] && cd $argv[1]
end

function lt -d "Like tree, except using `exa`."
    # Convenience wrapper around `exa --tree`.
    # Optionally accepts a number for the max-depth and a directory to list.
    # Usage: lt 2 ./scripts

    # lt
    if test (count $argv) -eq 0
        exa --tree --all

    else if test (count $argv) -eq 1
        # lt 2
        if string match --quiet --regex '^[0-9]+$' $argv[1]
            exa --tree --all --level $argv[1]

        # lt path/to/directory
        else if test -d $argv[1]
            exa --tree --all $argv[1]
        end

    # lt 2 path/to/directory
    else if test (count $argv) -eq 2
        exa --tree --all --level $argv[1] $argv[2]
    end

end

function lad -d "List only the directories within a directory."
    # TODO: Support $argv[1], which is currently broken here. See functions.zsh
    # for a reference.
    fd --hidden --maxdepth 1 --type d
end

function laf -d "List only the files within a directory."
    # TODO: Support $argv[1], which is currently broken here. See functions.zsh
    # for a reference.
    fd --hidden --maxdepth 1 --type f
end

function lal -d "List only the links within a directory."
    # TODO: Support $argv[1], which is currently broken here. See functions.zsh
    # for a reference.
    fd --hidden --maxdepth 1 --type l
end

function nix_introspect -d "Search through local nixpkgs repository."
    rg --after-context 5 "\\b$argv[1]\\b\\s*=" (nix-instantiate --find-file nixpkgs)
end
