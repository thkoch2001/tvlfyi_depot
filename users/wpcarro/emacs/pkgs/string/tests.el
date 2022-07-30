;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest string-caps->kebab ()
  (string= "foo-bar-baz" (string-caps->kebab "FOO_BAR_BAZ")))

(ert-deftest string-kebab->caps ()
  (string= "FOO_BAR_BAZ" (string-kebab->caps "foo-bar-baz")))

(ert-deftest string-lower->caps ()
  (string= "FOO_BAR_BAZ" (string-lower->caps "foo bar baz")))

(ert-deftest string-lower->kebab ()
  (string= "foo-bar-baz" (string-lower->kebab "foo bar baz")))
