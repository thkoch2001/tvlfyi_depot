;;; private/grfn/tests/splitjoin_test.el -*- lexical-binding: t; -*-

(require 'ert)
;; (load! 'splitjoin)
;; (load! 'utils)
; (require 'splitjoin)

;;; Helpers

(defvar *test-buffer* nil)
(make-variable-buffer-local '*test-buffer*)

(defun test-buffer ()
  (when (not *test-buffer*)
    (setq *test-buffer* (get-buffer-create "test-buffer")))
  *test-buffer*)

(defmacro with-test-buffer (&rest body)
  `(with-current-buffer (test-buffer)
     ,@body))

(defun set-test-buffer-mode (mode)
  (let ((mode (if (functionp mode) mode
                (-> mode symbol-name (concat "-mode") intern))))
    (assert (functionp mode))
    (with-test-buffer (funcall mode))))

(defmacro set-test-buffer-contents (contents)
  (with-test-buffer
   (erase-buffer)
   (insert contents)))

(defun test-buffer-contents ()
  (with-test-buffer (substring-no-properties (buffer-string))))

(defmacro assert-test-buffer-contents (expected-contents)
  `(should (equal (string-trim (test-buffer-contents))
                  (string-trim ,expected-contents))))

(defmacro should-join-to (mode original-contents expected-contents)
  `(progn
     (set-test-buffer-mode ,mode)
     (set-test-buffer-contents ,original-contents)
     (with-test-buffer (+splitjoin/join))
     (assert-test-buffer-contents ,expected-contents)))

(defmacro should-split-to (mode original-contents expected-contents)
  `(progn
     (set-test-buffer-mode ,mode)
     (set-test-buffer-contents ,original-contents)
     (with-test-buffer (+splitjoin/split))
     (assert-test-buffer-contents ,expected-contents)))

(defmacro should-splitjoin (mode joined-contents split-contents)
  `(progn
     (should-split-to ,mode ,joined-contents ,split-contents)
     (should-join-to  ,mode ,split-contents  ,joined-contents)))

;;; Tests

;; Elixir
(ert-deftest elixir-if-splitjoin-test ()
  (should-splitjoin 'elixir
   "if predicate?(), do: result"
   "if predicate?() do
  result
end"))

