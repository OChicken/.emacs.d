;;; init-package.el --- package management and keyring preparation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; package.el --- Simple package system for Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mandatory before installing packages

; (require 'transient)
; "Invalid slot name" issue
; https://emacs.stackexchange.com/a/50781
;; after magit update you don't need that

(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(defun package-install-init (package)
  "Install PACKAGE if not exists."
  (unless (package-installed-p package)
    (package-install package)))

(unless (package-installed-p 'gnu-elpa-keyring-update)
  ; gnu-elpa-keyring-update need special treatment since without the keyring
  ; you cannot install any packages.
  ; https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure
  (setq package-check-signature nil)
  (package-refresh-contents)
  (package-install 'gnu-elpa-keyring-update)
  (setq package-check-signature 'allow-unsigned) ; set back to default
)

(provide 'init-package)
;;; init-package.el ends here
