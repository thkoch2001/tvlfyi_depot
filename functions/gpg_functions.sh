#!/usr/bin/env zsh

function gpg-encrypt-dir() {
    dirname=$1
    tar -cz "$dirname" | gpg --symmetric --output "$dirname.tar.gz.gpg"
}


function gpg-decrypt-dir() {
    dirname=$1
    outdir=${dirname%.tar.gz.gpg}

    if [ -d "$outdir" ]; then
        echo "Output directory, $outdir, already exists and will be overwritten by this command. Aborting..."
        exit 1
    else
        gpg --decrypt $dirname | tar -xv
    fi

}
