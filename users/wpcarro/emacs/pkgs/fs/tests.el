;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'fs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest fs-test-ensure-file ()
  (let ((file "/tmp/file/a/b/c/file.txt"))
    ;; Ensure this file doesn't exist first to prevent false-positives.
    (f-delete file t)
    (fs-ensure-file file)
    (should (and (f-exists? file)
                 (f-file? file)))))

(ert-deftest fs-test-ensure-dir ()
  (let ((dir "/tmp/dir/a/b/c"))
    ;; Ensure the directory doesn't exist.
    (f-delete dir t)
    (fs-ensure-dir dir)
    (should (and (f-exists? dir)
                 (f-dir? dir)))))
