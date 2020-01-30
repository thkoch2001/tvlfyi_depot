;;; do.el --- Small assertion library for Elisp -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Assertion library inspired by Elixir's core testing library.
;;
;; The goal here is to create this module without relying on other non-core
;; Elisp libraries.  I will attempt to do this as long as I'm not sacrificing
;; the readability of this code nor the ease at which it can be written.
;;
;; A note on testing:
;; Another goal with this library is to blur the line between testing code and
;; runtime code.  Developers should ideally be using `do/assert' and `do/refute'
;; in their library code.  Because of this, I'm avoiding referring
;; to the notion of testing in the names of these functions.
;;
;; Hypothesis:
;; The lower the friction is for writing tests, the more likely people will
;; write tests.

;; TODO: Support better error messages, which might include information about
;; line numbers in source code where the assertion failed.

;; TODO: Consider offering the ability to have some of these functions compile
;; to nothing at runtime if developers want to use them while developing without
;; incurring the costs at runtime.

;; TODO: Consider using this module instead of prelude.el.  Right now, I'm
;; having troubling preferring one to the other.  The benefit of this module is
;; that it's independent of prelude, but that might also be a downside, since
;; the messaging that asserting should be a critical part of any core library
;; like prelude.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro do/assert (x)
  "Errors unless X is t.
These are strict assertions and purposely do not rely on truthiness."
  (let ((as-string (format "%s" x)))
    `(unless (equal t ,x)
       (error (concat "Assertion failed: " ,as-string)))))

(defmacro do/refute (x)
  "Errors unless X is nil."
  (let ((as-string (format "%s" x)))
    `(unless (eq nil ,x)
       (error (concat "Refutation failed: " ,as-string)))))

(provide 'do)
;;; do.el ends here
