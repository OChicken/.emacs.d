;;; init-lang.el --- Major modes for various languages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-package)

; crontab-mode - MELPA
; https://melpa.org/#/crontab-mode
(package-install-init 'crontab-mode)
(add-to-list 'auto-mode-alist '("\\.?cron\\(tab\\)?\\'" . crontab-mode))

(setq-local package-list
            '(cmake-mode
              cuda-mode
              cython-mode
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

; Markdown Mode for Emacs
; https://jblevins.org/projects/markdown-mode/
(package-install-init 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
  '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

; web-mode.el - html template editing for emacs
; https://web-mode.org/
(package-install-init 'web-mode)
(require 'css-mode)
(add-to-list 'html-mode-hook 'web-mode)


(provide 'init-lang)
;;; init-lang.el ends here
