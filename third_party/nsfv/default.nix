# Real-time Noise Suppression Plugin (for PulseAudio).
#
# This should be invoked as a "ladspa" plugin for pulseaudio.
#
# https://github.com/werman/noise-suppression-for-voice
{ pkgs, lib, ... }:

pkgs.stdenv.mkDerivation {
  name = "noise-suppression-for-voice";

  nativeBuildInputs = [ pkgs.cmake ];
  src = pkgs.fetchFromGitHub {
    owner = "werman";
    repo = "noise-suppression-for-voice";
    rev = "cd5c79378ab9819cd85f4fd108f3c77a40fd66ac";
    sha256 = "10zh1al1bys60sjdd4p72qbp9jfb5wq1zaw33b595psgwmqpbckq";
  };

  meta = with lib; {
    description = "Real-time noise suppression LADSPA plugin";
    license = licenses.gpl3;
  };
}
