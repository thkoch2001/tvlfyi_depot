function fish_prompt
    set_color blue
    echo -n (whoami)
    set_color cyan
    echo -n '@'
    set_color magenta
    echo -n (prompt_pwd)
    set_color cyan
    echo -n '> '
end

set PATH ~/Library/Haskell/bin ~/bin/ /Users/vincent/Source/management-scripts/google-apps/ $PATH
