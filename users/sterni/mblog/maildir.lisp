;; SPDX-License-Identifier: GPL-3.0-only
;; SPDX-FileCopyrightText: Copyright (C) 2022 by sterni

(in-package :maildir)
(declaim (optimize (safety 3)))

(defun list (dir)
  "Returns a list of pathnames to messages in a maildir. The messages are
  returned in no guaranteed order. Note that this function doesn't fully
  implement the behavior prescribed by maildir(5): It only looks at `cur`
  and `new` and won't clean up `tmp` nor move files from `new` to `cur`,
  since it is strictly read-only."
  (flet ((subdir-contents (subdir)
           (directory
            (merge-pathnames
             (make-pathname :directory `(:relative ,subdir)
                            :name :wild :type :wild)
             dir))))
    (mapcan #'subdir-contents '("cur" "new"))))

