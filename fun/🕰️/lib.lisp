(defpackage 🕰️
  (:use :cl)
  (:import-from :local-time
   :timestamp-subtimezone :*default-timezone* :sec-of)
  (:export :⌚))

(in-package :🕰️)
(declaim (optimize (safety 3)))

(defparameter *clock-emojis*
  (vector #\🕛 #\🕧   ; 00:00 - 00:30
          #\🕐 #\🕜   ; 01:00 - 01:30
          #\🕑 #\🕝   ; 00:00 - 00:30
          #\🕒 #\🕞   ; 00:00 - 00:30
          #\🕓 #\🕟   ; 00:00 - 00:30
          #\🕔 #\🕠   ; 00:00 - 00:30
          #\🕕 #\🕡   ; 00:00 - 00:30
          #\🕖 #\🕢   ; 00:00 - 00:30
          #\🕗 #\🕣   ; 00:00 - 00:30
          #\🕘 #\🕤   ; 00:00 - 00:30
          #\🕙 #\🕥   ; 00:00 - 00:30
          #\🕚 #\🕦)) ; 11:00 - 11:30

(defun ⌚ (timestamp &optional (tz *default-timezone*))
  "Convert a LOCAL-TIME:TIMESTAMP into the nearest Unicode clock face.
  Use TZ (which defaults to LOCAL-TIME:*DEFAULT-TIMEZONE*) to determine
  the UTC offset to factor when determining the local clock face."
  (let* ((offset (multiple-value-bind (offset-secs _dst _name)
                   (timestamp-subtimezone timestamp tz)
                   offset-secs))
         (secs (+ (sec-of timestamp) offset)))
    (elt *clock-emojis* (mod (round (/ secs 1800)) 24))))
