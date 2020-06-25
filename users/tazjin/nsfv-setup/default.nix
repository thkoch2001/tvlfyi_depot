# Configures a running Pulseaudio instance with an LADSP filter that
# creates a noise-cancelling sink.
#
# This can be used to, for example, cancel noise from an incoming
# video conferencing audio stream.
#
# There are some caveats, for example this will not distinguish
# between noise from different participants and I have no idea what
# happens if the default sink goes away.
{ pkgs, ... }:

let
  inherit (pkgs) ripgrep pulseaudio nsfv;
in pkgs.writeShellScriptBin "nsfv-setup" ''
  SINK=$(${pulseaudio}/bin/pacmd info | ${ripgrep}/bin/rg -r '$1' '^Default sink name: (.*)$')
  echo "Setting up NSFV filtering to sink ''${SINK}"
  ${pulseaudio}/bin/pacmd load-module module-ladspa-sink sink_name=NSFV sink_master=''${SINK} label=noise_suppressor_stereo plugin=${pkgs.nsfv}/lib/ladspa/librnnoise_ladspa.so control=50
''
