;;; ~/.doom.d/nix-yapf-mode.el -*- lexical-binding: t; -*-


(defun +grfn/yapfify-call-bin (input-buffer output-buffer start-line end-line)
  (with-current-buffer input-buffer
    (call-process-region
     (point-min)
     (point-max)
     "nix-shell"
     nil
     (list output-buffer nil)
     nil
     "/home/griffin/code/urb/grid/yapf.nix"
     "--run"
     (concat
      "yapf -l "
      (number-to-string start-line)
      "-"
      (number-to-string end-line)))))

(advice-add #'yapfify-call-bin :override #'+grfn/yapfify-call-bin)
