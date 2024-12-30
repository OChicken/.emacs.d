;;; init-view.el --- View settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-package)

;; diminish --- Diminished modes are minor modes with no modeline display ;;;;;
; https://github.com/myrjola/diminish.el

(package-install-init 'diminish)
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'hs-minor-mode)
(diminish 'visual-line-mode)

;; rainbow-delimiters: Emacs rainbow delimiters mode ;;;;;;;;;;;;;;;;;;;;;;;;;;
; A "rainbow parentheses"-like mode which highlights delimiters such as
; parentheses, brackets or braces according to their depth.
; https://github.com/Fanael/rainbow-delimiters
(package-install-init 'rainbow-mode)
(package-install-init 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode))


;; mode-line-bell: Flash the Emacs mode line instead of ringing the bell ;;;;;;
; Flash the Emacs mode line instead of ringing the bell
; https://github.com/purcell/mode-line-bell
(package-install-init 'mode-line-bell)
(mode-line-bell-mode t)


;; Display ugly ^L page breaks as tidy horizontal lines ;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/purcell/page-break-lines
(package-install-init 'page-break-lines)
(add-hook 'find-file-hook 'page-break-lines-mode)
(add-hook 'page-break-lines-mode-hook
          (lambda() (diminish 'page-break-lines-mode)))


;; Highlight escape sequences in Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/dgutov/highlight-escape-sequences

(package-install-init 'highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)
(put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
(put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face)


;; Vertical Interactive Completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/minad/vertico
; ATTENTION! Raise "'emacs-28.1' is unavailable" warning!

; (package-install-init 'vertico)
; (add-hook 'after-init-hook 'vertico-mode)


(provide 'init-view)
;;; init-view.el ends here
