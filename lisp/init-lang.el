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

(require 'markdown-mode)
(require 'markdown-indent)
(require 'md-ts-mode)
(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
  '("\\.\\(?:md\\|markdown\\)\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook #'markdown-indent-mode)
(setq-default markdown-hide-markup t ; Make bold/italic actually render visually
	      markdown-hide-urls t
	      markdown-fontify-whole-heading-line t
	      markdown-header-scaling t)
; No variable-height headers: all six levels stay at the body size.
; Note both variables have a :set that rebuilds the header faces, so they
; must be set with customize-set-variable, not setq -- and they travel
; together: leaving scaling at t without values falls back to markdown's
; stock (2.0 1.7 1.4 1.1 1.0 1.0), i.e. h1 at double size.
; (customize-set-variable 'markdown-header-scaling nil)

; The tree-sitter markdown / markdown-inline grammar recipes are not
; registered here: md-ts-mode already adds both, pinned to v0.4.1.

(require 'valign)
(add-hook 'markdown-mode-hook #'valign-mode)


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

(require 'pdf-annot)
(setq pdf-annot-default-annotation-properties
      '((t (label . ,user-full-name))
        (text (icon . "Note")
              (color . "#ff0000"))
        (highlight (color . "yellow")
                   (opacity . 0.5))
        (squiggly (color . "orange"))
        (strike-out (color . "red"))
        (underline (color . "blue"))))


;; web-mode --- html template editing for emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://web-mode.org/

(require 'css-mode)
(add-to-list 'html-mode-hook 'web-mode)


;; jasmin-mode --- A minimal major mode for the Jasmin DSL ;;;;;;;;;;;;;;;;;;;;

(require 'jasmin-mode)


;; Verilog ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'verilog-mode)
(add-to-list 'auto-mode-alist '("\\.v\\'" . verilog-mode))


(provide 'init-lang)
;;; init-lang.el ends here
