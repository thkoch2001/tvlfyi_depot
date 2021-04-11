;;; -*- lexical-binding: t; -*-

(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

(defun packer-format-buffer ()
  (interactive)
  (let ((buf (get-buffer-create "*packer-fmt*")))
    (if (zerop (call-process "packer" nil buf nil "fmt" (buffer-file-name)))
        (revert-buffer t t t)
      (message "packer fmt failed: %s" (with-current-buffer buf (buffer-string))))))

(define-minor-mode packer-format-on-save-mode
  "Run packer-format-buffer before saving the current buffer"
  :lighter nil
  (if packer-format-on-save-mode
      (add-hook 'before-save-hook #'packer-format-buffer nil t)
    (remove-hook 'before-save-hook #'packer-format-buffer t)))

(defun maybe-init-packer ()
  (interactive)
  (when (s-ends-with-p ".pkr" (file-name-base (buffer-file-name)))
    (packer-format-on-save-mode)))

(add-hook 'hcl-mode-hook #'maybe-init-packer)
