functon npms() {
  clear;
  npm start;
}

# custom js functions
source $HOME/pc_settings/.js_to_bash.sh

# custom git functions
source $HOME/pc_settings/.git_functions.sh

# custom bash helpers functions
source $HOME/pc_settings/.misc_functions.sh

# generates placeholder content for FE work
function lorem {
    text="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

    echo $text
}

# generates lorem and adds to pasteboard
function loremcp {
    lorem | pbcopy
}

# searches all PATH directories for a keyword
function wsearchpath {
  echo $PATH | tr ':' '\n' | xargs -I {} find {} -type f -perm +111 -maxdepth 1 -name "*${1}*" -print | xargs basename
}

