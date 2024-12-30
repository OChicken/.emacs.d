;;; init-edit.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some basic preferences ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq set-mark-command-repeat-pop t
;;       truncate-partial-width-windows nil)

;; (transient-mark-mode t)

;; flyspell.el --- On-the-fly spell checker ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "aspell")
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))


;; ispell.el --- interface to spell checkers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ispell)
(setq ispell-dictionary "en")


;; anzu -- Displays current match and total matches info in the mode-line ;;;;;
; https://github.com/emacsorphanage/anzu

(package-install-init 'anzu)
(global-anzu-mode +1)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)
(set-face-attribute 'anzu-mode-line nil
                    :foreground "white"
                    :weight 'bold)
(diminish 'anzu-mode)


;; imenu-list --- Show the current buffer's imenu entries in a separate buffer
; https://github.com/bmag/imenu-list

(package-install-init 'imenu-list)
(require 'imenu-list)
(setq imenu-list-size 50)


;; Search for and list unicode characters in Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/purcell/list-unicode-display
(package-install-init 'list-unicode-display)


;; move-dup --- Moving and duplications of lines or selections ;;;;;;;;;;;;;;;;
; https://github.com/wyuenho/move-dup

(package-install-init 'move-dup)
; When paredit is enabled (e.g. in org mode), it will use those keybindings
; M-up and M-down. Therefore, you might prefer to use M-S-up and M-S-down,
; which will work even in lisp modes.
(global-set-key (kbd "M-S-<up>")   'move-dup-move-lines-up)
(global-set-key (kbd "M-S-<down>") 'move-dup-move-lines-down)


;; multiple-cursors --- Multiple cursors for emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/magnars/multiple-cursors.el

(package-install-init 'multiple-cursors)
(global-set-key (kbd "C-c C-SPC") 'mc/mark-pop)
(global-set-key (kbd "C-M-l")   'mc/mark-next-like-this)     ; origin: reposition-window
(global-set-key (kbd "C-M-S-l") 'mc/mark-previous-like-this) ; origin: recenter-other-window
(global-set-key (kbd "C-M-w")   'mc/mark-next-like-this-word)      ; origin: append-next-kill
(global-set-key (kbd "C-M-S-w") 'mc/mark-previous-like-this-word)  ; origin: NULL
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)


;; symbol-overlay --- Highlight symbols with keymap-enabled overlays ;;;;;;;;;;
; https://github.com/wolray/symbol-overlay

(package-install-init 'symbol-overlay)
(require 'symbol-overlay)
(dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
  (add-hook hook 'symbol-overlay-mode))
(with-eval-after-load 'symbol-overlay
  (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))
(diminish 'symbol-overlay-mode)


;; which-key --- Displays available keybindings in popup ;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/justbur/emacs-which-key

(package-install-init 'which-key)
(require 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(which-key-setup-side-window-right)
(setq which-key-idle-delay 5
      which-key-popup-type 'side-window)
(diminish 'which-key-mode)


;; wrap-region --- Wrap text with punctation or tag ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/rejeep/wrap-region.el

(package-install-init 'wrap-region)
(require 'wrap-region)
(wrap-region-global-mode t)
(wrap-region-add-wrappers
 '(("\(" "\)" nil c-mode)
   ("\"" "\"" nil c-mode)
   ("*" "*" nil org-mode)
   ("/" "/" nil org-mode)
   ("~" "~" nil org-mode)
   ("=" "=" nil org-mode)
   ("+" "+" nil org-mode)))
(diminish 'wrap-region-mode)


;; xclip --- Copy&paste GUI clipboard from text terminal ;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/emacsmirror/xclip/tree/master

(when (or (getenv "DISPLAY")
          (getenv "WAYLAND_DISPLAY"))
  (package-install-init 'xclip)
  (xclip-mode t))


;; yasnippet --- A template system for Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/joaotavora/yasnippet

(package-install-init 'yasnippet)
(yas-global-mode 1)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(diminish 'yas-minor-mode)


;; yasnippet-snippets --- a collection of yasnippet snippets for many languages
; https://github.com/AndreaCrotti/yasnippet-snippets

(package-install-init 'yasnippet-snippets)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             VC: Version Control                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It's Magit! A Git Porcelain inside Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://magit.vc/

(package-install-init 'magit)
(require 'magit)
(global-unset-key (kbd "C-x g")) ; magit-status (use "magit" to open it instead)
(setq magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv))
(global-set-key (kbd "C-x g m") 'magit)


;; Emacs port of GitGutter which is Sublime Text Plugin ;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/emacsorphanage/git-gutter/

(package-install-init 'git-gutter)
(global-git-gutter-mode +1)
; A long lasting bug: git-gutter annotation disappeared during buffer switching.
; https://github.com/emacsorphanage/git-gutter/issues/155
(global-set-key (kbd "C-x g g") 'git-gutter)
(global-set-key (kbd "C-x g =") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-x g n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-x g p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x g s") 'git-gutter:stage-hunk)
(global-set-key (kbd "C-x g r") 'git-gutter:reverse-hunk)
(global-set-key (kbd "C-x g SPC") 'git-gutter:mark-hunk)
(diminish 'git-gutter-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Org mode                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'org-indent-mode)

;;;;;;;;;;;;
;; Agenda ;;
;;;;;;;;;;;;

(require 'org-agenda)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))

(require 'org-capture)
; Fast note taking in Org
; file:///usr/share/emacs/29.1/lisp/org/org-capture.el.gz
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/Documents/gtd.org")
(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* TODO %?\nCLOCK: %U\n " :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\nCLOCK: %a\n " :clock-resume t)
        ))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(setq org-agenda-files (list "~/Documents/gtd.org")
      org-agenda-compact-blocks t
      org-agenda-sticky t
      org-agenda-start-on-weekday 0
      org-agenda-span 'day
      org-agenda-include-diary nil
      org-agenda-window-setup 'current-window  ; make org-agenda open up in the current window
      org-agenda-sorting-strategy
      '((agenda habit-down time-up user-defined-up effort-up category-keep)
        (todo category-up effort-up)
        (tags category-up effort-up)
        (search category-up)))


;;;;;;;;;;
;; Link ;;
;;;;;;;;;;

(require 'ol)
; Org links library
; file:///usr/share/emacs/29.1/lisp/org/ol.el.gz
(define-key global-map (kbd "C-c l") 'org-store-link)

; Insert org-mode links from clipboard
; https://github.com/rexim/org-cliplink
(package-install-init 'org-cliplink)
(require 'org-cliplink)
(global-set-key (kbd "C-c y") 'org-cliplink)
(setq org-cliplink-max-length 120)  ; cuts any title that exceeds the limit


;;;;;;;;;;;;;;;;
;; Org Refile ;;
;;;;;;;;;;;;;;;;

(require 'org-refile)
; Refile Org Subtrees
; file:///usr/share/emacs/29.1/lisp/org/org-refile.el.gz

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;;;;;;;;;;
;; Babel ;;
;;;;;;;;;;;

(require 'org-src)
; Source code examples in Org
; file:///usr/share/emacs/29.1/lisp/org/org-src.el.gz
(setq org-edit-src-content-indentation 0)

(require 'ob)
; Working with Code Blocks in Org
; file:///usr/share/emacs/29.1/lisp/org/ob.el.gz

(setq-local package-list
            '(ob-go
              ob-php
              ob-sagemath))
(dolist (package package-list)
  (package-install-init package))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((C . t)
      (C++ . t)
      (calc . t)
      (ditaa . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (go . t)
      (js .t)
      (latex . t)
      (lisp . t)
      (octave . t)
      (php . t)
      (python . t)
      (rust . t)
      (sagemath . t)
      (shell . t)
      (sql . t)))))

(require 'ob-core)
; Working with Code Blocks
; file:///usr/share/emacs/29.1/lisp/org/ob-core.el.gz
(setq org-confirm-babel-evaluate nil) ;; Do not confirm before evaluation

(require 'ob-latex)
; Babel Functions for LaTeX
; file:///usr/share/emacs/29.1/lisp/org/ob-latex.el.gz
; (setq org-babel-latex-pdf-svg-process "dvisvgm --pdf %f -o %O")

(require 'ob-ditaa)
(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

(require 'ob-sagemath)
; org-babel integration with SageMath
; https://github.com/sagemath/ob-sagemath
(package-install-init 'ob-sagemath)
(set-variable 'sage-shell:use-prompt-toolkit nil)  ; for Ipython >=7 (sage-shell-mode.el) (same below)
(set-variable 'sage-shell:use-simple-prompt  t)
(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)


;;;;;;;;;;;;;;;;;;;
;; LaTeX preview ;;
;;;;;;;;;;;;;;;;;;;

(require 'org)
; Outline-based notes management and organizer
; file:///usr/share/emacs/29.1/lisp/org/org.el.gz

; adjust the compiler of imagemagick to XeLaTeX and use it as default process to
; convert LaTeX fragments to image files
(let ((pos (assoc 'imagemagick org-preview-latex-process-alist)))
  (plist-put (cdr pos) :latex-compiler '("xelatex -interaction nonstopmode -output-directory %o %f")))
(setq org-preview-latex-default-process 'imagemagick)  ; origin: dvipng

(require 'org-fragtog)
; Automatically toggle Org mode LaTeX fragment previews as the cursor enters and exits them
; https://github.com/io12/org-fragtog
(package-install-init 'org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode) ; for latex live previous


;;;;;;;;;;;;
;; Export ;;
;;;;;;;;;;;;

(require 'ox)
; Export Framework for Org Mode
; file:///usr/share/emacs/29.1/lisp/org/ox.el.gz
(setq org-export-with-tags nil  ; dont export headlines with tags
      org-export-coding-system 'utf-8
      org-html-validation-link nil
      org-export-with-broken-links t  ; who cares about annoying broken link errors..
      )

(require 'ox-latex)
; LaTeX Back-End for Org Export Engine
; file:///usr/share/emacs/29.1/lisp/org/ox-latex.el.gz
(setq org-latex-compiler "xelatex"                    ; origin: "pdflatex"
      org-latex-image-default-width ".5\\linewidth")  ; origin: ".9\\linewidth"

(add-to-list 'org-latex-classes
             '("ox-latex-scrarticle"
               "\\documentclass[a4paper, headsepline, footsepline]{scrarticle}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-html-htmlize-output-type 'css)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 Utils: Elisp helper functions and commands                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kill-dired-buffers ()
  "Kill all Dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i Dired buffer(s)." count))))

(defun kill-magit-buffers ()
  "Kill all Magit buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'magit-diff-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'magit-process-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'magit-revision-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
	(when (equal major-mode 'magit-log-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i Magit buffer(s)." count))))

(defun kill-log-buffers ()
  "Kill all log/error buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'fundamental-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'emacs-lisp-compilation-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'xref--xref-buffer-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'package-menu-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'flycheck-error-list-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'flycheck-error-message-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'TeX-output-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'TeX-special-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'compilation-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'gud-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'completion-list-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'grep-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'occur-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'ob-sagemath-error-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'ediff-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'diff-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i Output Log buffer(s)." count))))

(defun kill-gnus-buffers ()
  "Kill buffers of Gnus."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'message-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'gnus-summary-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'gnus-article-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i Gnus buffer(s)." count))))

(defun kill-el-gz-buffers ()
  "Kill all buffers with filenames ending in '.el.gz'."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (and (buffer-file-name)
                   (string-match "\\.el\\.gz\\'" (buffer-file-name)))
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i *.el.gz file(s)." count))))


(provide 'init-edit)
;;; init-edit.el ends here
