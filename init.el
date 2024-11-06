;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(when (version<= emacs-version "24.5.1")
  ; Many features are not avail at and before Emacs 24.5.1 (Ubuntu 16.04 LTS)
  (warn "Your Emacs %s is TOO old: MANY features in this config are not \
available. Please upgrade if possible." emacs-version))

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(require 'init-lite)

; An accurate port of the default Visual Studio Code Dark+ theme for Emacs
; https://github.com/ianyepan/vscode-dark-plus-emacs-theme
; Modified by me
; Features in this theme is NOT available at Emacs 24.5.1 on Ubuntu 16.04 LTS
; So I mark it available for the Emacs 25.2.2 on Ubuntu 18.04.5 LTS

(unless (version< emacs-version "25.2.2")
  (customize-set-variable 'vscode-dark-plus-box-org-todo nil)
  (load-theme 'vscode-dark t))


(add-to-list 'load-path (expand-file-name "lisp/math" user-emacs-directory))

;; Load configs for specific features and modes

(let ((lite-mode (member "--lite" command-line-args)))
  (setq command-line-args (delete "--lite" command-line-args))
  (when (not lite-mode)
    (require 'init-lang)
    (require 'init-view)
    (require 'init-edit)
    (require 'init-progmodes)
    (require 'init-org)
    (require 'init-utils)
    (require 'init-opt)   ; comment this since this is personal
    (require 'init-gnus)  ; comment this since this is personal
    )
  )

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
