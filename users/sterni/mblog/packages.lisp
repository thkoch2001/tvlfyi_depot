;; SPDX-License-Identifier: GPL-3.0-only
;; SPDX-FileCopyrightText: Copyright (C) 2022-2023 by sterni

(defpackage :maildir
  (:use :common-lisp)
  (:shadow :list)
  (:export :list)
  (:documentation
   "Very incomplete package for dealing with maildir(5)."))

(defpackage :config
  (:use
   :common-lisp)
  (:import-from :uiop :getenv)
  (:import-from :alexandria :when-let)
  (:export
   :init-from-env
   :*general-buffer-size*))

(defpackage :note
  (:use
   :common-lisp
   :closure-html
   :cl-date-time-parser
   :mime4cl
   :config)
  (:import-from
   :alexandria
   :when-let*
   :when-let
   :starts-with-subseq
   :ends-with-subseq)
  (:import-from :who :escape-string-minimal)
  (:export
   :apple-note
   :apple-note-uuid
   :apple-note-subject
   :apple-note-time
   :apple-note-text-part
   :make-apple-note
   :apple-note-html-fragment))

(defpackage :mblog
  (:use
   :common-lisp
   :klatre
   :who
   :maildir
   :note
   :config)
  (:export :build-mblog)
  (:import-from :local-time :universal-to-timestamp)
  (:import-from :mime4cl :redirect-stream)
  (:shadowing-import-from :common-lisp :list))

(defpackage :cli
  (:use
   :common-lisp
   :uiop
   :note
   :config
   :mblog)
  (:import-from :alexandria :starts-with)
  (:export :main))
