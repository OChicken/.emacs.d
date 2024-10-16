;;; init-view.el --- View settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common view settings (of both TTY frames & GUI frames) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Attention: the variables re-defined here are all defined in 'C source code'
;; of Emacs, so that you must use 'setq-default' instead of 'setq'.


(setq-default indicate-buffer-boundaries 'left
              display-fill-column-indicator-character ?\u254e
(add-hook 'find-file-hook 'display-fill-column-indicator-mode)


; Diminished modes are minor modes with no modeline display
; https://github.com/myrjola/diminish.el
(package-install-init 'diminish)

(require 'time)
; display time, load and mail indicator in mode line of Emacs
; file:///usr/share/emacs/29.3/lisp/time.el.gz
;; (dolist (format '(tab-bar-format-align-right tab-bar-format-global))
;;   (add-to-list 'tab-bar-format format t))
(setq display-time-format "%F %T %z")
(setq display-time-interval 1)
;; (display-time-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elements in a frame ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(diminish 'visual-line-mode)

; Fanael/rainbow-delimiters: Emacs rainbow delimiters mode
; A "rainbow parentheses"-like mode which highlights delimiters such as
; parentheses, brackets or braces according to their depth.
; https://github.com/Fanael/rainbow-delimiters
(package-install-init 'rainbow-mode)
(package-install-init 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode))

; purcell/mode-line-bell: Flash the Emacs mode line instead of ringing the bell
; Flash the Emacs mode line instead of ringing the bell
; https://github.com/purcell/mode-line-bell
(package-install-init 'mode-line-bell)
(mode-line-bell-mode t)

; Display ugly ^L page breaks as tidy horizontal lines
; https://github.com/purcell/page-break-lines
(package-install-init 'page-break-lines)
(add-hook 'git-gutter-mode-hook 'page-break-lines-mode)
(diminish 'page-break-lines-mode)

(package-install-init 'highlight-escape-sequences)
; Highlight escape sequences in Emacs
; https://github.com/dgutov/highlight-escape-sequences
(add-hook 'after-init-hook 'hes-mode)
(put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
(put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face)

(package-install-init 'vertico)
; VERTical Interactive COmpletion
; https://github.com/minad/vertico
; (add-hook 'after-init-hook 'vertico-mode)
(global-set-key (kbd "C-x C-M-v") 'vertico-mode)


(provide 'init-view)
;;; init-view.el ends here
