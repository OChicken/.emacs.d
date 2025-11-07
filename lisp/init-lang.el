;;; init-lang.el --- Major modes for various languages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; crontab-mode - MELPA --- Major mode for crontab(5) files ;;;;;;;;;;;;;;;;;;;
; https://melpa.org/#/crontab-mode

(add-to-list 'auto-mode-alist '("\\.?cron\\(tab\\)?\\'" . crontab-mode))


;; csv-mode --- Major mode for editing comma/char separated values ;;;;;;;;;;;;

(require 'csv-mode)
(add-hook 'csv-mode-hook 'csv-align-fields)


;; dockerfile-mode --- An emacs mode for handling Dockerfile ;;;;;;;;;;;;;;;;;;

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("/Dockerfile[^/]*\\'" . dockerfile-mode))


;; markdown-mode --- Markdown Mode for Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://jblevins.org/projects/markdown-mode/

(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
  '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


;; Emacs support library for PDF files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/vedang/pdf-tools

; If you are the first time to install it, don't forget to execute
; pdf-tools-install to activate it.
(require 'pdf-tools)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(add-to-list 'display-line-numbers-exceptions-alist 'pdf-view-mode)
(add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
(define-key pdf-view-mode-map (kbd "q") 'pdf-view-previous-page-command) ; quit-window (global)
(define-key pdf-view-mode-map (kbd "z") 'pdf-view-next-page-command) ; undefined


;; web-mode --- html template editing for emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://web-mode.org/

(require 'css-mode)
(add-to-list 'html-mode-hook 'web-mode)


;; jasmin-mode --- A minimal major mode for the Jasmin DSL ;;;;;;;;;;;;;;;;;;;;

(require 'jasmin-mode)


(provide 'init-lang)
;;; init-lang.el ends here
