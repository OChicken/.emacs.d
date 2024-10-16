;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(require 'init-lite)

(add-to-list 'load-path (expand-file-name "lisp/math" user-emacs-directory))



;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Bootstrap config
(require 'init-package)  ; Machinery for installing required packages

;; Load configs for specific features and modes
(require 'init-lang)
(require 'init-view)       ; rainbow
(require 'init-edit)       ; multi-line edit, spell check, git, dir navigation


(let ((lite-mode (member "--lite" command-line-args)))
  (setq command-line-args (delete "--lite" command-line-args))
  (when (not lite-mode)
    (require 'init-sessions)   ; recentf, session, desktop
    (require 'init-progmodes)  ; Flycheck, auto-complete
    (require 'init-org)
    (require 'init-utils)
    (require 'init-opt)        ; Wakatime, sunshine
    (require 'init-gnus))
  )

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
