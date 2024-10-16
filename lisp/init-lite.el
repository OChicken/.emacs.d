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
;;; - "z" kbds like `C-z' (suspend-frame), `M-z' (zap-to-char) and
;;; - "q" kbds like `C-q' (quoted-insert), `M-q' (fill-paragraph)
;;; are barely used, so I exploit them to the <down> and <up> respectively.
;;;
;;; Perhaps `C-z' the suspend-frame, is also, to some extend, a frequently used
;;; command.  Actually, suspend-frame bounds to both `C-z' and "C-x C-z", so
;;; that you can still use the latter one.
;;;
;;; Notice that, `M-e' (forward-sentence) is also barely used.  Instead, an
;;; <RET> for left-hand is desired, so `M-e' where "e" for "enter" is the
;;; suitable name.
;;;
;;; So that you will see the ubiquitous "z", "q", "e" in this config.  By the
;;; above settings, you can do almost everything solely with your left hand.
;;; For example, during compiling, you will do
;;; 1. `M-!': shell-command, revoke minibuffer-local-shell-command-map
;;; 2. `M-q' (instead of `M-p'): previous-history-element in the minibuffer
;;; 3. `M-e' (instead of `RET'): hit `<return>'
;;;
;;; Code:

(when (version<= emacs-version "27.1")
  ; "26.1" is the version number appears in Purcell's config on 20230905.
  ; If this file is suitable for an older Emacs version, I shall decrease this
  ; number.
  (warn "Your Emacs %s is old, and some functionality in this config will be \
disabled. Please upgrade if possible." emacs-version))



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

(define-key minibuffer-local-map (kbd "M-q") 'previous-history-element) ; fill-paragraph
(define-key minibuffer-local-map (kbd "M-z") 'next-history-element)     ; zap-to-char



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


;; custom.el --- tools for declaring and initializing options ;;;;;;;;;;;;;;;;;

; An accurate port of the default Visual Studio Code Dark+ theme for Emacs
; https://github.com/ianyepan/vscode-dark-plus-emacs-theme
; Modified by me
(customize-set-variable 'vscode-dark-plus-box-org-todo nil)
(load-theme 'vscode-dark t)


;; delsel.el --- delete selection if you insert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(delete-selection-mode t)


;; dired.el --- directory-browsing commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)
(setq dired-listing-switches "-alFo --time-style=long-iso")


;; display-line-numbers.el --- interface for display-line-numbers ;;;;;;;;;;;;;

(setq-default display-line-numbers-width 4)
(add-hook 'find-file-hook
          (lambda ()
            (unless (memq major-mode '(doc-view-mode
				       image-mode
				       grep-mode
				       eshell-mode))
              (display-line-numbers-mode t))))


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


;; frame.el --- multi-frame management independent of window systems ;;;;;;;;;;

(blink-cursor-mode t)
(window-divider-mode t) ; so that you can adjust the width of left/right window

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
; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur)


;; mouse.el --- window system-independent mouse support ;;;;;;;;;;;;;;;;;;;;;;;

(setq mouse-yank-at-point t)


;; mwheel.el --- Mouse wheel support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mouse-wheel-scroll-amount
      '(3 ((shift)  . hscroll)
	  ((meta))
	  ((control meta) . global-text-scale)
	  ((control) . text-scale))
      mouse-wheel-progressive-speed nil)  ; don"t accelerate scrolling


;; newcomment.el --- (un)comment regions of buffers ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-M-c") 'comment-dwim) ; undefined


;; scroll-bar.el --- window system-independent scroll bar support ;;;;;;;;;;;;;

(set-scroll-bar-mode 'left)


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
(global-set-key (kbd "C-a") 'beginning-of-line)   ; beginning-of-visual-line
(global-set-key (kbd "C-e") 'end-of-line)         ; end-of-visual-line
(global-set-key (kbd "M-a") 'back-to-indentation) ; backward-sentence
(global-set-key (kbd "M-e") (kbd "RET"))          ; forward-sentence
(global-set-key (kbd "M-S-e") (kbd "C-o"))
(global-set-key (kbd "M-#") 'undo)                ; undefined
(global-set-key (kbd "C-x C-q") 'quoted-insert)   ; read-only-mode
(global-set-key (kbd "C-x C-r") 'read-only-mode)  ; find-file-read-only
(global-set-key (kbd "C-z") 'next-line)           ; suspend-frame
(global-set-key (kbd "C-q") 'previous-line)       ; quoted-insert
(global-set-key (kbd "C-M-z") 'scroll-up-line)    ; suspend-frame
(global-set-key (kbd "C-M-q") 'scroll-down-line)  ; quoted-insert


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
                 (progn (end-of-line) (point))))
(global-set-key (kbd "C-x C-d") 'delete-logic-line) ; list-directory


;; tool-bar.el --- setting up the tool bar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1) ; disable (It's ugly and provides limited functionalities)


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


;; uniquify.el --- unique buffer names dependent on file name ;;;;;;;;;;;;;;;;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "@"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")


;; window.el --- GNU Emacs window commands aside from those written in C ;;;;;;

(defun recenter-top-bottom-hl (&optional arg)
  "Call `recenter-top-bottom' in the current window and highlight pulse it.
With numeric prefix ARG, move current line to window-line ARG."
  (interactive "P")
  (recenter-top-bottom arg)
  (pulse-momentary-highlight-one-line (point)))
(global-set-key (kbd "C-l") 'recenter-top-bottom-hl)

(global-set-key (kbd "C-x C-M-q") 'previous-buffer)   ; undefined
(global-set-key (kbd "C-x C-M-z") 'next-buffer)       ; undefined
(global-set-key (kbd "C-x C-v") 'end-of-buffer)       ; find-alternative-file
(global-set-key (kbd "C-x M-v") 'beginning-of-buffer) ; undefined

; Toggling among windows. This modifies the default "C-<tab>" and "C-S-<tab>"
; behaviour, that toggling among tabs. In practice, toggling among tabs is
; barely used. You can still use tab-next command via "C-x t o".

(global-set-key (kbd "C-<tab>") 'other-window) ; tab-next
(global-set-key (kbd (if (window-system)
                         "C-S-<iso-lefttab>"
                       "C-<tab>"))
                (lambda (n)
                  (interactive "p")
                  (other-window (- n)))) ; tab-previous

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

(require 'em-hist)
(if (version<= emacs-version "27.1")
    (warn "`eshell-hist-mode-map' is void due to your old Emacs.")
  (progn
    (define-key eshell-hist-mode-map (kbd "M-q")
                'eshell-previous-matching-input-from-input)
    (define-key eshell-hist-mode-map (kbd "M-z")
                'eshell-next-matching-input-from-input)))


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


;; inf-lisp.el --- an inferior-lisp mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'inf-lisp)
(when (executable-find "sbcl")
  (setq inferior-lisp-buffer "sbcl"))


;; prog-mode.el --- Generic major mode for programming ;;;;;;;;;;;;;;;;;;;;;;;;

(define-key prog-mode-map       (kbd "C-M-z") 'scroll-up-line)
(define-key prog-mode-map       (kbd "C-M-q") 'scroll-down-line)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           lisp/textmodes/           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; flyspell.el --- On-the-fly spell checker ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "aspell")
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))


;; ispell.el --- interface to spell checkers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ispell)
(setq ispell-dictionary "en")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               lisp/vc/              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ediff.el --- a comprehensive visual interface to diff & patch ;;;;;;;;;;;;;;

(require 'ediff)
(with-eval-after-load 'ediff
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))



(provide 'init-lite)
;;; init-lite.el ends here
