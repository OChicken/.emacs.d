;;; init-lite.el --- Lite settings -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; I call it `lite' because I just want to keep the basic functionalities on
;;; the legacy OS or pure tty machines.  No packages will be installed on this
;;; config.
;;;
;;;
;;; Code:

(let ((minver "26.1"))
  ; "26.1" is the version number appears in Purcell's config on 20230905.
  ; If this file is suitable for an older Emacs version, I shall decrease this
  ; number.
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))



(provide 'init-lite)
;;; init-lite.el ends here
