;;; macros.el --- Helpful variables for making my ELisp life more enjoyable -*- lexical-binding: t -*-
;; Authpr: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This file contains helpful variables that I use in my ELisp development.

;; TODO: Consider a macro solution for mimmicking OCaml's auto resolution of
;; dependencies using `load-path' and friends.

;;; Code:

(require 'f)
(require 'string)
(require 'symbol)

;; TODO: Support `xi' lambda shorthand macro.

(defmacro enable (mode)
  "Helper for enabling `MODE'.
Useful in `add-hook' calls.  Some modes, like `linum-mode' need to be called as
`(linum-mode 1)', so `(add-hook mode #'linum-mode)' won't work."
  `#'(lambda nil (,mode 1)))

(defmacro disable (mode)
  "Helper for disabling `MODE'.
Useful in `add-hook' calls."
  `#'(lambda nil (,mode -1)))

(defmacro add-hooks (modes callback)
  "Add multiple `MODES' for the `CALLBACK'.
Usage: (add-hooks '(one-mode-hook 'two-mode-hook) #'fn)"
  `(dolist (mode ,modes)
     (add-hook mode ,callback)))

(defmacro add-hook-before-save (mode f)
  "Register a hook, `F', for a mode, `MODE' more conveniently.
Usage: (add-hook-before-save 'reason-mode-hook #'refmt-before-save)"
  `(add-hook ,mode
             (lambda ()
               (add-hook 'before-save-hook ,f))))

;; TODO: Debug.
(defmacro macros/ilambda (&rest body)
  "Surrounds `BODY' with an interactive lambda function."
  `(lambda ()
     (interactive)
     ,@body))

;; TODO: Privatize?
(defun namespace ()
  "Return the namespace for a function based on the filename."
  (->> (buffer-file-name)
       f-filename
       f-base))

(defmacro macros/comment (&rest _)
  "Empty comment s-expresion where `BODY' is ignored."
  `nil)

;; NOTE: Not prepending the "macros" to this macro, since brevity is its goal.
(defmacro >> (&rest forms)
  "Compose a new, point-free function by composing FORMS together."
  (let ((sym (gensym)))
    `(lambda (,sym)
       (->> ,sym ,@forms))))

;; TOOD: Support this.
(cl-defmacro macros/test
    (&key function
          test
          args
          expect
          equality)
  (let* ((namespace (namespace))
         (test-name (string/->symbol
                     (s-concat namespace
                               "/"
                               "test"
                               "/"
                               (s-chop-prefix
                                (s-concat namespace "/")
                                (symbol/to-string function))))))
    `(ert-deftest ,test-name ()
       ,test
       (should (,equality (apply ,function ,args)
                        ,expect)))))

(defmacro macros/support-file-extension (ext mode)
  "Register MODE to automatically load with files ending with EXT extension.
Usage: (macros/support-file-extension \".pb\" protobuf-mode)"
  (let ((extension (string/format "\\.%s\\'" ext)))
    `(add-to-list 'auto-mode-alist '(,extension . ,mode))))

(provide 'macros)
;;; macros.el ends here
