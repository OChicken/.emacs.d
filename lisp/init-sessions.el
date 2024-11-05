;;; init-sessions.el --- Keep track of the recent files, history and sessions. -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; This config need not to install any packages, but keeping track the sessions
;;; are vital to backup your works if you want to do development or any other
;;; works on your personal computer.
;;;
;;; Code:

; keep track of recently opened files
; file:///usr/share/emacs/29.1/lisp/recentf.el.gz
(require 'recentf)
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-saved-items 100
      recentf-exclude `("/tmp/" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))

; Save minibuffer history
; file:///usr/share/emacs/29.1/lisp/savehist.el.gz
(add-hook 'after-init-hook 'savehist-mode)


(provide 'init-sessions)
;;; init-sessions.el ends here
