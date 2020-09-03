
function reset-audio --description "Resets pulse and alsa"
    pulseaudio -k
    sudo alsa force-reload
end
