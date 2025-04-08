;;; init-lang.el --- Major modes for various languages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-package)

;; crontab-mode - MELPA --- Major mode for crontab(5) files ;;;;;;;;;;;;;;;;;;;
; https://melpa.org/#/crontab-mode

(package-install-init 'crontab-mode)
(add-to-list 'auto-mode-alist '("\\.?cron\\(tab\\)?\\'" . crontab-mode))

(setq-local package-list
            '(cmake-mode
              cuda-mode
              cython-mode
              dockerfile-mode
              go-mode
              gnuplot
              gnuplot-mode
              htmlize
              magma-mode
              php-mode
              rust-mode
              sage-shell-mode
              yaml
              yaml-mode))
(dolist (package package-list)
  (package-install-init package))

;; markdown-mode --- Markdown Mode for Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://jblevins.org/projects/markdown-mode/

(package-install-init 'markdown-mode)
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
(package-install-init 'pdf-tools)
(require 'pdf-tools)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(add-to-list 'display-line-numbers-exceptions-alist 'pdf-view-mode)
(add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)


;; web-mode --- html template editing for emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://web-mode.org/

(package-install-init 'web-mode)
(require 'css-mode)
(add-to-list 'html-mode-hook 'web-mode)


;; jasmin-mode --- A minimal major mode for the Jasmin DSL ;;;;;;;;;;;;;;;;;;;;

(require 'jasmin-mode)


(provide 'init-lang)
;;; init-lang.el ends here
