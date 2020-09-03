
function prodaccess
    ssh-add "$HOME/.ssh/id_ecdsa_sk"
    begin; ssh-add -L | grep -q 'ZgEu6S3SLatYN'; end || ssh-add "$HOME"/.ssh/id_ed25519
    begin; ssh-add -L | grep -q 'Gfh2S3kUwZ8A6'; end || ssh-add "$HOME"/.ssh/id_rsa.discourse
    echo "signing test" | gpg --clearsign > /dev/null
end
