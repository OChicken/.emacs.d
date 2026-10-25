;;; init-desktop.el --- desktop session related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       lisp/ (desktop session related)                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; recentf.el --- keep track of recently opened files ;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'recentf)
(add-hook 'after-init-hook 'recentf-mode)


;; desktop.el --- Save partial status of Emacs when killed ;;;;;;;;;;;;;;;;;;;;

(require 'desktop)
(setq desktop-path `(,user-emacs-directory)
      desktop-load-locked-desktop 'check-pid
      desktop-globals-to-save
      '((comint-input-ring        . 50)
        (compile-history          . 30)
        desktop-missing-file-warning
        (dired-regexp-history     . 20)
        (extended-command-history . 30)
        (face-name-history        . 20)
        (file-name-history        . 100)
        (grep-find-history        . 30)
        (grep-history             . 30)
        (ivy-history              . 100)
        (magit-revision-history   . 50)
        (minibuffer-history       . 50)
        (org-clock-history        . 50)
        (org-refile-history       . 50)
        (org-tags-history         . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        register-alist
        (search-ring              . 20)
        (shell-command-history    . 50)
        ))
; Closing emacs results in "Current desktop was not loaded from a file" even
; though desktop-save-mode was set before start
; https://emacs.stackexchange.com/a/66822
(unless (daemonp)
  (desktop-change-dir "."))
(desktop-save-mode t)
(desktop-read)

(provide 'init-desktop)
;;; init-desktop.el ends here
