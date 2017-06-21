#!/usr/bin/env zsh

function gpg-encrypt-dir() {
    dirname=$1
    echo "Encrypting..."
    tar -cz "$dirname" | gpg --symmetric --output "$dirname.tar.gz.gpg"
    echo "Done."
}


function gpg-decrypt-dir() {
    dirname=$1
    outdir=${dirname%.tar.gz.gpg}

    if [ -d "$outdir" ]; then
        echo "Output directory, $outdir, already exists and will be overwritten by this command. Aborting..."
        exit 1
    else
        echo "Decrypting..."
        gpg --decrypt $dirname | tar -xv
        echo "Done."
    fi

}


# WIP
function gpg-encrypt-file() {
    filename=$1
    echo "Encrypting..."
    gpg --symmetric $filename
    echo "Done."
}


# WIP
function gpg-decrypt-file() {
    filename=$1
    echo "Decrypting..."
    gpg --decrypt $filename >"${filename%.gpg}"
    echo "Done."
}
