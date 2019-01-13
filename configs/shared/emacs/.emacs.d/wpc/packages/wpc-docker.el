;;; docker.el --- Docker preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; My Docker preferences and configuration

;;; Code:

(use-package docker
  :config
  (setenv "DOCKER_TLS_VERIFY" "1")
  (setenv "DOCKER_HOST" "tcp://10.11.12.13:2376")
  (setenv "DOCKER_MACHINE_NAME" "name"))

(provide 'wpc-docker)
;;; wpc-docker.el ends here
