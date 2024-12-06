;;; init-lite.el --- Lite settings -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; I call it "lite" because I just want to keep the basic functionalities on
;;; the legacy OS and pure tty machines.  NO PACKAGES WILL BE INSTALLED on this
;;; config.  Besides, this config is tailored to my LEFT-HANDED QUIRK, so use
;;; it at your own risk 🙂
;;;
;;; Noticed that the original
;;; - "z" kbds like "C-z" (suspend-frame), "M-z" (zap-to-char) and
;;; - "q" kbds like "C-q" (quoted-insert), "M-q" (fill-paragraph)
;;; are barely used, so I exploit them to the <down> and <up> respectively.
;;;
;;; Perhaps "C-z" the suspend-frame, is also, to some extend, a frequently used
;;; command.  Actually, suspend-frame bounds to both "C-z" and "C-x C-z", so
;;; that you can still use the latter one.
;;;
;;; Notice that, "M-e" (forward-sentence) is also barely used.  Instead, an
;;; <RET> for left-hand is desired, so "M-e" where "e" for "enter" is the
;;; suitable name.
;;;
;;; So that you will see the ubiquitous "z", "q", "e" in this config.  By the
;;; above settings, you can do almost everything solely with your left hand.
;;; For example, during compiling, you will do
;;; 1. "M-!": shell-command, revoke minibuffer-local-shell-command-map
;;; 2. "M-q" (instead of "M-p"): previous-history-element in the minibuffer
;;; 3. "M-e" (instead of "RET"): hit `<return>'
;;;
;;; The HIGH RISK for you to know is, I bind "C-x C-c" to "comment-dwim" so you
;;; cannot use this shortcut to close Emacs; instead, use "C-x C-M-c" to close
;;; Emacs (deliberately add some inconvenience to exit Emacs is nice to your
;;; ongoing work 🙂)
;;;
;;; For your convenience, I markdown the Emacs version of various Ubuntu LTS so
;;; that you know which features are NOT support on your Ubuntu.
;;; - Ubuntu 16.04 LTS: Emacs 24.5.1
;;; - Ubuntu 18.04 LTS: Emacs 25.2.2
;;; - Ubuntu 20.04 LTS: Emacs 26.3
;;; - Ubuntu 22.04 LTS: Emacs 27.1
;;;
;;; Code:

(when (version<= emacs-version "27.1")
  ; Some features are not avail at and before Emacs 27.1 (Ubuntu 22.04.5 LTS)
  (message "Your Emacs %s is old: some features in this config are not \
available. Please upgrade if possible." emacs-version))



;; C source code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq create-lockfiles nil
      frame-resize-pixelwise t
      frame-title-format '((:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                    "%b")))
      scroll-preserve-screen-position t
      use-file-dialog nil
      use-dialog-box nil
      window-resize-pixelwise t)

; The following variables have to set `default' to make global
(setq-default cursor-type 'bar
              fill-column 80
              indicate-buffer-boundaries t)

; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(define-key minibuffer-local-map (kbd "M-q") 'previous-history-element) ; fill-paragraph
(define-key minibuffer-local-map (kbd "M-z") 'next-history-element)     ; zap-to-char
(global-set-key (kbd "C-x C-M-w") 'kill-buffer)                         ; undefined



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                lisp/                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; autorevert.el --- revert buffers when files on disk change ;;;;;;;;;;;;;;;;;

(global-auto-revert-mode t)
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)


;; comint.el --- general command interpreter in a window stuff ;;;;;;;;;;;;;;;;

(require 'comint)
(define-key comint-mode-map (kbd "M-q") 'comint-previous-input)
(define-key comint-mode-map (kbd "M-z") 'comint-next-input)


;; delsel.el --- delete selection if you insert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(delete-selection-mode t)


;; dired.el --- directory-browsing commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)
(setq dired-listing-switches "-alFo --time-style=long-iso")


;; display-fill-column-indicator.el --- interface for display-fill-column-indicator
; This feature is available at least Emacs version >= 27.1 (Ubuntu 22.04.5 LTS)

(unless (version< emacs-version "27.1")
  (add-hook 'find-file-hook 'display-fill-column-indicator-mode))


;; display-line-numbers.el --- interface for display-line-numbers ;;;;;;;;;;;;;
; This feature is available at least Emacs version >= 26.1 according to doc
; So this feature is available for the Emacs 26.3 on Ubuntu 20.04 LTS

(defvar display-line-numbers-exceptions-alist '(doc-view-mode
                                                image-mode
                                                grep-mode
                                                eshell-mode))
(defun display-line-numbers-mode-exceptions ()
  "Display line numbers, EXCEPT for the following modes."
  (unless (memq major-mode display-line-numbers-exceptions-alist)
    (display-line-numbers-mode t)))

(unless (version< emacs-version "26.1")
  (setq-default display-line-numbers-width 4)
  (add-hook 'find-file-hook 'display-line-numbers-mode-exceptions))


; elec-pair.el --- Automatic parenthesis pairing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'elec-pair)
(electric-pair-mode   t)  ; paired parentheses, brackets, and quotes
(electric-indent-mode t)  ; adjust indentation according to the context


;; faces.el --- Lisp faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'default nil
                    :family  "Hack"
                    :foundry "SRC"
                    :slant   'normal
                    :weight  'regular
                    :height  68     ; (Hack 7: height 68; Hack 8: height 83)
                    :width   'normal)


;; files.el --- file input and output commands for Emacs ;;;;;;;;;;;;;;;;;;;;;;

(setq auto-save-default nil
      make-backup-files nil) ; stop generating a.txt~ after you edit it

(add-to-list 'auto-mode-alist '("\\.emacs\\.grep\\'"   . grep-mode))
(add-to-list 'auto-mode-alist '("\\.emacs\\.eshell\\'" . eshell-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(add-to-list 'auto-mode-alist '("\\Makefile\\(?:\\..*\\)\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\(rc\\|_\\w+\\)\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.styl\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

; I want a quick comment cmd for my left-hand quirk. Some days ago I use
; "C-x C-M-c" to do so, but very easily to hit "C-x C-c" that close the Emacs.
; So I bind "C-x C-M-c" to close Emacs, and bind "C-x C-c" to comment-dwim.
; Check the position "newcomment.el" for detail.
(global-set-key (kbd "C-x C-M-c") 'save-buffers-kill-terminal) ; undefined


;; fill.el --- fill commands for Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unfill-region (start end)
  "Transform a multi-line region between START & END into a single line."
  (interactive "r")
  (let ((fill-column (point-max)))
    (fill-region start end nil)))


;; frame.el --- multi-frame management independent of window systems ;;;;;;;;;;

(blink-cursor-mode t)

; This feature is available at least Emacs version >= 25.1 according to doc
; So this feature is available for the Emacs 25.2.2 on Ubuntu 18.04.5 LTS
(unless (version< emacs-version "25.1")
  (window-divider-mode t)) ; you can adjust the width of left/right window


; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;; hl-line.el --- highlight the current line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-hl-line-mode t)


;; ibuffer --- operate on buffers like dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ibuffer)
(require 'ibuf-ext)
(with-eval-after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size)))
  ;; Modify the default ibuffer-formats (toggle with `)
  (setq ibuffer-formats '((mark modified read-only vc-status-mini " "
                                (name 22 22 :left :elide)
                                " "
                                (size-h 9 -1 :right)
                                " "
                                (mode 14 14 :left :elide)
                                " "
                                vc-relative-file)
                          (mark modified read-only vc-status-mini " "
                                (name 22 22 :left :elide)
                                " "
                                (size-h 9 -1 :right)
                                " "
                                (mode 14 14 :left :elide)
                                " "
                                (vc-status 12 12 :left)
                                " "
                                vc-relative-file))
        ibuffer-filter-group-name-face 'font-lock-doc-face
        ibuffer-show-empty-filter-groups nil))
(global-set-key (kbd "C-x C-b") 'ibuffer)  ; list-buffers


;; ido.el --- interactively do things with buffers and files ;;;;;;;;;;;;;;;;;;

(ido-mode t)


;; isearch.el --- incremental search minor mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
(define-key isearch-mode-map (kbd "M-q") 'isearch-ring-retreat)
; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur)
(unless (version< emacs-version "27.1")
  (setq isearch-lazy-count t))


;; mouse.el --- window system-independent mouse support ;;;;;;;;;;;;;;;;;;;;;;;

(setq mouse-yank-at-point t)


;; mwheel.el --- Mouse wheel support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mouse-wheel-scroll-amount
      '(3 ((shift)  . hscroll)
	  ((meta))
	  ((control meta) . global-text-scale)
	  ((control) . text-scale))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling


;; newcomment.el --- (un)comment regions of buffers ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-c") 'comment-dwim) ; undefined


;; outline.el --- outline mode commands for Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(outline-minor-mode t)


;; savehist.el --- Save minibuffer history ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'savehist-mode)


;; scroll-bar.el --- window system-independent scroll bar support ;;;;;;;;;;;;;

(when (display-graphic-p)
  (set-scroll-bar-mode 'left))


;; simple.el --- basic editing commands for Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq save-interprogram-paste-before-kill t)

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(global-set-key (kbd "M-d")'delete-word) ; kill-word

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))
(global-set-key (kbd "C-<backspace>")'backward-delete-word) ; backward-kill-word

(defun zap-up-to-nonspace-char ()
  "Zap up to non-space char."
  (interactive)
  (while (eq (char-after) ?\s)
    (delete-char 1)))
(global-set-key (kbd "M-z") 'zap-up-to-nonspace-char) ; zap-to-char

(global-visual-line-mode t)
(column-number-mode t)
; ATTENTION: C-a, C-e, M-a, M-e are heavily modified
(global-set-key (kbd "M-a") 'beginning-of-line)   ; backward-sentence
(global-set-key (kbd "C-a") 'back-to-indentation) ; beginning-of-visual-line
(global-set-key (kbd "C-e") 'end-of-line)         ; end-of-visual-line
(global-set-key (kbd "M-e") (kbd "RET"))          ; forward-sentence
(global-set-key (kbd "M-E") (kbd "C-o"))
; Other shortcuts
(global-set-key (kbd "M-#") 'undo)                ; undefined
(global-set-key (kbd "C-x C-q") 'quoted-insert)   ; read-only-mode
(global-set-key (kbd "C-x C-r") 'read-only-mode)  ; find-file-read-only
(global-set-key (kbd "C-z") 'next-line)           ; suspend-frame
(global-set-key (kbd "C-q") 'previous-line)       ; quoted-insert
(global-set-key (kbd "C-M-z") 'scroll-up-line)    ; suspend-frame
(global-set-key (kbd "C-M-q") 'scroll-down-line)  ; quoted-insert
(global-set-key (kbd "C-x C-v") 'end-of-buffer)       ; find-alternative-file
(global-set-key (kbd "C-x M-v") 'beginning-of-buffer) ; undefined


;; startup.el --- process Emacs shell arguments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq initial-scratch-message
      (concat initial-scratch-message
              ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

;; subr.el --- basic lisp subroutines for Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'yes-or-no-p 'y-or-n-p) ; y-or-n instead of yes-or-no

(defun delete-logic-line ()
  "Delete the logic line."
  (interactive)
  (delete-region (progn (beginning-of-line) (point))
                 (progn (forward-line 1) (point))))
(global-set-key (kbd "C-x C-d") 'delete-logic-line) ; list-directory


;; tab-bar.el --- frame-local tabs with named persistent window configurations
; This feature is available at least Emacs version >= 27.1 (Ubuntu 22.04.5 LTS)

(unless (version< emacs-version "27.1")
  (tab-bar-mode t)
  ; (set-face-attribute 'tab-bar nil :height 100)
  (global-set-key (kbd "C-<prior>") 'tab-bar-switch-to-prev-tab) ; scroll-right
  (global-set-key (kbd "C-<next>")  'tab-bar-switch-to-next-tab) ; scroll-left
)


;; thingatpt.el --- get the `thing' at point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mark-symbol-at-point ()
  "Mark the symbol at point."
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (when sym
        (forward-symbol -1)
        (mark-sexp)
        (message "Mark set"))))
(global-set-key (kbd "C-x C-M-SPC") 'mark-symbol-at-point)

(defun copy-word-at-point ()
  "Copy the word at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (when word
        (kill-new word)
        (message "Copied '%s' to kill ring" word))))
(global-set-key (kbd "C-x M-w") 'copy-word-at-point)


;; time.el --- display time, load and mail indicator in mode line of Emacs ;;;;

(require 'time)
; (dolist (format '(tab-bar-format-align-right tab-bar-format-global))
;   (add-to-list 'tab-bar-format format t))
(setq display-time-format "%F %T %z")
(setq display-time-interval 1)
; (display-time-mode)


;; tool-bar.el --- setting up the tool bar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (display-graphic-p)
  (tool-bar-mode -1)) ; disable: It's ugly and provides limited functionalities


;; uniquify.el --- unique buffer names dependent on file name ;;;;;;;;;;;;;;;;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "@"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")


;; window.el --- GNU Emacs window commands aside from those written in C ;;;;;;

; There are several times that I hit 'C-x 1' by accident and cause catastrophe
; issue --- deleted my window layout, so I must mute this shortcut
(global-unset-key (kbd "C-x 1")) ; delete-other-windows

(defun recenter-top-bottom-hl (&optional arg)
  "Call `recenter-top-bottom' in the current window and highlight pulse it.
With numeric prefix ARG, move current line to window-line ARG."
  (interactive "P")
  (recenter-top-bottom arg)
  (pulse-momentary-highlight-one-line (point)))
(global-set-key (kbd "C-t") 'recenter-top-bottom-hl) ; transpose-chars
; When I hit 'C-y' to paste, its easily hit 'C-t' by accident. However, the
; 'C-t' that 'transpose-chars' is barely used and this accident actions requires
; me to undo. So I bound 'C-t' to this innocent (but even more fancy) feature.

(global-set-key (kbd "C-x C-M-q") 'previous-buffer) ; undefined
(global-set-key (kbd "C-x C-M-z") 'next-buffer)     ; undefined

; Toggling among windows. This modifies the default "C-<tab>" and "C-S-<tab>"
; behaviour, that toggling among tabs. In practice, toggling among tabs is
; barely used. You can still use tab-next command via "C-x t o".

(defun other-window-previous (count)
  "Select another window in cyclic ordering of windows previously.
COUNT specifies the number of windows to skip, starting with the
selected window, before making the selection."
  (interactive "p")
  (other-window (- count)))
(global-set-key (kbd "C-<tab>") 'other-window) ; tab-next
(global-set-key (kbd (if (window-system)
                         "C-S-<iso-lefttab>"
                       "C-S-<tab>"))
                'other-window-previous)        ; tab-previous

(defvar last-window (list (selected-window) nil)
  "Last selected window.")

(defun store-current-and-last-window (_frame)
  "Store the current and last window in the form (current last).
The `last-last' window will be pop out if this function is executed again."
  (setq last-window (butlast last-window))
  (setq last-window (cons (selected-window) last-window)))

(defun select-last-window ()
  "Jump to the last window."
  (interactive)
  (select-window (cadr last-window)))

(add-hook 'window-selection-change-functions 'store-current-and-last-window)

(global-set-key (kbd "C-x C-<tab>") 'select-last-window)

(defun scroll-up-half ()
  "Scroll text of selected window upward for half window."
  (interactive)
    (move-to-window-line -1)
    (recenter))

(defun scroll-down-half ()
  "Scroll text of selected window down for half window."
  (interactive)
    (move-to-window-line 0)
    (recenter))


;; xt-mouse.el --- support the mouse when emacs run in an xterm ;;;;;;;;;;;;;;;

(xterm-mouse-mode t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;             lisp/eshell/            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; em-hist.el --- history list management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This feature is NOT available at and before Emacs <= 27.1 (Ubuntu 22.04.5 LTS)

(require 'em-hist)
(unless (version<= emacs-version "27.1")
  (define-key eshell-hist-mode-map (kbd "M-q")
              'eshell-previous-matching-input-from-input)
  (define-key eshell-hist-mode-map (kbd "M-z")
              'eshell-next-matching-input-from-input))


;; em-prompt.el --- command prompts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'em-prompt)
(setq eshell-prompt-function
      (lambda ()
        (concat (abbreviate-file-name (eshell/pwd)) "\n"
                (format-time-string "%T" (current-time))
                (if (= (user-uid) 0) " # " " $ "))))


;; em-unix.el --- UNIX command aliases ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'em-unix)
; https://emacs.stackexchange.com/questions/57714/how-to-keep-grep-results-in-eshell-buffer
(setq eshell-plain-grep-behavior t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           lisp/progmodes/           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cc-mode.el --- major mode for editing C and similar languages ;;;;;;;;;;;;;;

(require 'cc-mode)
(setq c-default-style "linux")
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook
	    (lambda()
	      (local-set-key (kbd "C-c C-c") 'compile)          ; comment-region
	      (local-set-key (kbd "M-e") (kbd "RET"))           ; c-end-of-statement
	      (local-set-key (kbd "M-a") 'beginning-of-line)))) ; c-beginning-of-statement


;; elisp-mode.el --- Emacs Lisp mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key emacs-lisp-mode-map (kbd "C-M-z") 'scroll-up-line)   ; undefined
(define-key emacs-lisp-mode-map (kbd "C-M-q") 'scroll-down-line) ; indent-pp-sexp


;; grep.el --- run `grep' and display the results ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'grep)
(setq grep-use-null-device nil
      grep-command "grep --color=auto -nr -F --exclude-dir=.git
--exclude-dir=fs --exclude-dir=drivers --exclude-dir=Documentation
--exclude-dir=net --exclude=TAGS --exclude=.emacs.desktop ")


;; hideshow.el --- minor mode cmds to selectively display code/comment blocks ;

(require 'hideshow)
(add-hook 'prog-mode-hook 'hs-minor-mode)  ; Enable code folding
(setq hs-hide-comments-when-hiding-all nil
      hs-isearch-open t)
(global-set-key (kbd "C-x 1") 'hs-hide-block) ; delete-other-windows
(global-set-key (kbd "C-x !") 'hs-show-block) ; undefined


;; inf-lisp.el --- an inferior-lisp mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'inf-lisp)
(cond
 ((executable-find "sbcl")
  (setq inferior-lisp-program "sbcl"))
 ((executable-find "ecl")
  (setq inferior-lisp-program "ecl"))
 (t
  (setq inferior-lisp-program "lisp")))


;; prog-mode.el --- Generic major mode for programming ;;;;;;;;;;;;;;;;;;;;;;;;

(define-key prog-mode-map (kbd "C-M-z") 'scroll-up-line)
(define-key prog-mode-map (kbd "C-M-q") 'scroll-down-line)
(prettify-symbols-mode t) ; so that "lambda" becomes the pretty symbol
(add-hook 'find-file-hook 'prettify-symbols-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               lisp/vc/              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ediff.el --- a comprehensive visual interface to diff & patch ;;;;;;;;;;;;;;

(require 'ediff)
(with-eval-after-load 'ediff
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  lisp/org/                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org.el --- Outline-based notes management and organizer ;;;;;;;;;;;;;;;;;;;;

(require 'org)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)) ; origin: backward-up-list
(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-imenu-depth 3            ; The maximum level for Imenu access to Org headlines.
      org-support-shift-select t   ; make shift-cursor commands select text when possible
      org-log-done 'time           ; Information to record when a task moves to the DONE state.
      org-hide-emphasis-markers t  ; font-lock should hide the emphasis marker characters.
      org-tags-column -80          ; The column to which tags should be indented in a headline.
      org-fast-tag-selection-single-key 'expert)

(require 'org-element)
(defun org-toggle-inline-image-at-point ()
  "Display inline image at point.
https://emacs.stackexchange.com/a/64640"
  (interactive)
  (let* ((context (org-element-context (org-element-at-point)))
         (type (org-element-type context))
         (beg  (plist-get (cadr context) :begin))
         (end  (plist-get (cadr context) :end)))
     (when (eq type 'link)
        (org-toggle-inline-images nil beg end))))
(define-key org-mode-map (kbd "C-c C-x M-v") 'org-toggle-inline-image-at-point)


(provide 'init-lite)
;;; init-lite.el ends here
