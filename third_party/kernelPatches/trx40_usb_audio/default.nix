# This patch adds the ASUS TRX40 Prime Pro Whatever Edition
# motherboard to the list of boards for which the USB Audio connector
# map has been fixed.
{ ... }:

{
  name = "trx40_usb_audio";
  patch = ./trx40_usb_audio.patch;
}
