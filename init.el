;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(when (version<= emacs-version "24.5.1")
  (error "Your Emacs %s is TOO OLD --- this config should be at least higher \
than this one, so please use the newer version e.g. the Emacs 25.2.2 (on \
Ubuntu 18.04 LTS.)" emacs-version))

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(require 'init-lite)

(add-to-list 'load-path (expand-file-name "lisp/math" user-emacs-directory))



;; Bootstrap config

;; Load configs for specific features and modes

(let ((lite-mode (member "--lite" command-line-args)))
  (setq command-line-args (delete "--lite" command-line-args))
  (when (not lite-mode)
    (require 'init-sessions)   ; recentf, session, desktop
    (require 'init-dev)
    (require 'init-progmodes)  ; Flycheck, auto-complete
    (require 'init-org)
    (require 'init-utils)
    (require 'init-opt)        ; Wakatime, sunshine
    (require 'init-gnus)
    )
  )

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
