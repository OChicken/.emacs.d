;;; blog.el --- generate and deploy settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(package-initialize)
(require 'ox-html)
(require 'ox-publish)

; How to remove message `Indentation setup for shell type sh`
; https://emacs.stackexchange.com/q/52846
(advice-add 'sh-set-shell :around
            (lambda (orig-fun &rest args)
              (let ((inhibit-message t))
                (apply orig-fun args))))

(require 'python)
(setq python-indent-guess-indent-offset nil)

;; ox-html.el --- HTML Backend for Org Export Engine ;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ox-html)
(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://OChicken.net/static/main.css\"/><style type=\"text/css\">  #content { text-align: left; } </style>"
      org-html-preamble  "<header><nav></nav></header>"
      org-html-postamble "<script>var toc = document.getElementById('table-of-contents'); document.querySelector('nav').appendChild(toc);</script>")

;; ox.el --- Export Framework for Org Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ox)
(setq org-export-with-author nil
      org-export-time-stamp-file nil ; timestamp
      org-export-with-section-numbers nil ; num
      org-export-preserve-breaks t) ; \n

;; ox-publish.el --- Publish Related Org Mode Files as a Website ;;;;;;;;;;;;;;

(require 'ox-publish)
(setq org-publish-project-alist
  (list (list "preview"
              :base-directory "."
              :publishing-directory "."
              :recursive nil
	      ; :exclude "demos/*"
              :publishing-function 'org-html-publish-to-html)))

(org-publish-project "preview" t)

(provide 'blog)
;;; blog.el ends here
