;;; wpc-dotnet.el --- C# and company -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Windows things v0v.

;;; Code:

(require 'macros)

(use-package csharp-mode)
(macros-support-file-extension "csproj" xml-mode)

(provide 'wpc-dotnet)
;;; wpc-dotnet.el ends here
