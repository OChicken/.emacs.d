;;; init-progmodes.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;   lisp/ (desktop session related)   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; recentf.el --- keep track of recently opened files ;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'recentf)
(add-hook 'after-init-hook 'recentf-mode)


;; desktop.el --- Save partial status of Emacs when killed ;;;;;;;;;;;;;;;;;;;;

(require 'desktop)
(setq desktop-path `(,user-emacs-directory)
      desktop-load-locked-desktop 'check-pid
      desktop-globals-to-save
      '((comint-input-ring        . 50)
        (compile-history          . 30)
        desktop-missing-file-warning
        (dired-regexp-history     . 20)
        (extended-command-history . 30)
        (face-name-history        . 20)
        (file-name-history        . 100)
        (grep-find-history        . 30)
        (grep-history             . 30)
        (ivy-history              . 100)
        (magit-revision-history   . 50)
        (minibuffer-history       . 50)
        (org-clock-history        . 50)
        (org-refile-history       . 50)
        (org-tags-history         . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        register-alist
        (search-ring              . 20)
        (shell-command-history    . 50)
        ))
; Closing emacs results in "Current desktop was not loaded from a file" even
; though desktop-save-mode was set before start
; https://emacs.stackexchange.com/a/66822
(desktop-change-dir ".")
(desktop-save-mode t)
(desktop-read)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              Projectile             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Project Interaction Library for Emacs
; https://github.com/bbatsov/projectile
(projectile-mode +1)
; Recommended keymap prefix on Windows/Linux
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
; A list of files considered to mark the root of a project
(dolist (file '(".gitignore" ".dir-locals.el" "compile_commands.json"))
  (add-to-list 'projectile-project-root-files-bottom-up file t))
(diminish 'projectile-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ibuffer: operate on buffers according to git or projectile ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We have two choices when using ibuffer: filter by either .git or project

;; Let Emacs' ibuffer-mode group files by git project etc., and show file state
; https://github.com/purcell/ibuffer-vc

;; Group buffers in Emacs ibuffer-mode by their projectile root directory ;;;;;
; https://github.com/purcell/ibuffer-projectile
(require 'ibuffer-projectile)

;; Set up the preferred filter.
(defun ibuffer-set-up-preferred-filters ()
  "Let ibuffer setup preferred filters.
Use either
  (ibuffer-vc-set-filter-groups-by-vc-root)
or
  (ibuffer-projectile-set-filter-groups)
Feel free to use command to toggle between them."
  (ibuffer-projectile-set-filter-groups)
  ;(ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))
(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;   Flycheck syntax checker settings  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flycheck)
; Flycheck --- Syntax checking for GNU Emacs --- Flycheck 33-cvs documentation
; https://www.flycheck.org/en/latest/
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'flycheck-mode))
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(setq flycheck-emacs-lisp-load-path 'inherit)

(setq-default flycheck-disabled-checkers '(python-pylint))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;                 Xref                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'xref)
; Cross-referencing commands
; file:///usr/share/emacs/29.1/lisp/progmodes/xref.el.gz
(setq tags-table-list '("~/.emacs.d/ctags/TAGS"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;              Completion             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; company-mode for Emacs
; https://company-mode.github.io/
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-tooltip-align-annotations t
      company-idle-delay 0.0
      company-show-quick-access t  ; Use M-1、M-2 to choose
      company-selection-wrap-around t
      company-transformers '(company-sort-by-occurrence))
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-z") 'company-select-next)
  (define-key company-active-map (kbd "M-q") 'company-select-previous))
(diminish 'company-mode)

(add-to-list 'company-backends 'company-c-headers)

;; company-math
(add-to-list 'company-backends 'company-math-symbols-latex)
(add-to-list 'company-backends 'company-math-symbols-unicode)

(company-auctex-init)

(require 'company-org-block)
; xenodium/company-org-block
; https://github.com/xenodium/company-org-block
(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-org-block)))

(defun company-files-no-space-post-completion ()
  "Disable space insertion after file name completion in Eshell."
  )

(with-eval-after-load 'company
  (defun company-files-eshell ()
    (when (derived-mode-p 'eshell-mode)
      (setq-local company-backends
                  '((company-files :with company-dabbrev-code)))
      ; Disable auto-add-space after completion
      (setq-local company-files--post-completion
                  #'company-files-no-space-post-completion)))
  (add-hook 'eshell-mode-hook #'company-files-eshell))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 lsp-bridge                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path (expand-file-name "elpa/lsp-bridge" user-emacs-directory))
;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Claude                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'claudemacs)
(define-key prog-mode-map (kbd "C-c m") #'claudemacs-transient-menu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Copilot                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'copilot)

(dolist (hook '(prog-mode-hook
                emacs-lisp-mode-hook
                org-mode-hook
                coq-mode-hook
                gnuplot-mode-hook
                latex-mode-hook)) ; the list is basically copied from init-edit.el
  (add-hook hook 'copilot-mode))
(dolist (hook '(yaml-mode-hook
                web-mode-hook)) ; the list is basically copied from init-edit.el
  (add-hook hook (lambda () (setq copilot-mode nil))))
(define-key copilot-completion-map (kbd "C-M-<tab>") 'copilot-accept-completion)
(add-to-list 'copilot-indentation-alist '(prog-mode 2))
(add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
(add-to-list 'copilot-indentation-alist '(coq-mode 2))
(add-to-list 'copilot-indentation-alist '(org-mode 2))
(add-to-list 'copilot-indentation-alist '(text-mode 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   GPT.el                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A simple LLM client for Emacs
; https://github.com/karthink/gptel
(require 'gptel)
(setq gptel-api-key (getenv "GPTEL-API-KEY"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Font                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface font-lock-hex-num-face
  '((t :foreground "#756cbd"))
  "Face for 0x-based hex constants.")
(add-hook 'prog-mode-hook
	  (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\b0x[0-9A-Fa-f]+\\b" 0 'font-lock-hex-num-face t)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Emacs Lisp                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (hook '(lisp-mode-hook lisp-interaction-mode-hook))
  (add-hook hook 'display-line-numbers-mode))


; Respawn the scratch buffer when it's killed
; https://github.com/jpkotta/immortal-scratch
(require 'immortal-scratch)
(add-hook 'after-init-hook 'immortal-scratch-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Lisp                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory)
                                "elpa/slime-20250918.2258/contrib"))

(define-key lisp-mode-map (kbd "C-M-q") 'scroll-down-line) ; indent-sexp
(add-hook 'lisp-mode-hook
          (lambda ()
            (unless (featurep 'slime)
              (require 'slime)
              (normal-mode))))

(require 'slime-repl)

(with-eval-after-load 'slime
  (define-key slime-repl-mode-map (kbd "M-q") 'slime-repl-previous-input)
  (define-key slime-repl-mode-map (kbd "M-z") 'slime-repl-next-input))

;; From http://bc.tech.coop/blog/070515.html
(defun lispdoc ()
  "Search lispdoc.com for SYMBOL: the symbol currently under the cursor."
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
                                                   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))
(define-key lisp-mode-map (kbd "C-c l") 'lispdoc)

(slime-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Haskell                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'haskell)
(require 'haskell-mode-autoloads)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'subword-mode)
(add-hook 'haskell-cabal-mode 'subword-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     Coq                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'proof-general)
(add-hook 'coq-mode-hook
          (lambda ()
            (define-key coq-mode-map (kbd "M-e") (kbd "RET"))))  ; proof-forward-command
(require 'proof-useropts)
(setq proof-three-window-enable nil)

(add-to-list 'auto-mode-alist '("\\.ec\\'" . coq-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    C/C++                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cc-mode)
(define-key c-mode-map (kbd "C-M-q") 'scroll-down-line)
(setq flycheck-clang-language-standard "gnu11")

(defun c-format-style ()
  "Format the current buffer with clang-format using the specified style file."
  (interactive)
  (when (and buffer-file-name (string-match "\\.c\\'" buffer-file-name))
    ;; Save the buffer if it's modified
    (when (buffer-modified-p)
      (save-buffer))
    ;; Call clang-format with the specified style
    (let ((temp (read-char-choice
                 "Select style: (1) Linux, (2) NIST, (3) Default (LLVM) "
                 '(?1 ?2 ?3))))
      (cond
       ((eq temp ?1)
        (call-process "clang-format" nil nil nil
		      (concat "-style=file:"
			      (expand-file-name user-emacs-directory)
			      "clang-format/Linux")
		      "-i" (buffer-name)))
       ((eq temp ?2)
        (call-process "clang-format" nil nil nil
                      (concat "-style=file:"
                              (expand-file-name user-emacs-directory)
                              "clang-format/NIST")
                      "-i" (buffer-name)))
       (t
        (call-process "clang-format" nil nil nil
                      "-style=LLVM"
                      "-i" (buffer-name)))))
    (revert-buffer t t t)))
(define-key c-mode-map (kbd "C-c C-f") 'c-format-style)

;; lss (the disassemble of elf) should be text mode in order to enable symbol-overlay-mode
(add-to-list 'auto-mode-alist '("\\.lss\\'" . text-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Python                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'python)
; Python's flying circus support for Emacs
; file:///usr/share/emacs/29.1/lisp/progmodes/python.el.gz
(setq python-indent-offset 4)
(define-key python-mode-map (kbd "C-M-f") 'python-nav-forward-sexp)  ; forward-sexp
(define-key python-mode-map (kbd "C-M-b") 'python-nav-backward-sexp) ; backward-sexp
(add-hook 'python-mode-hook (lambda () (eldoc-mode -1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Maxima                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/usr/share/emacs/site-lisp/maxima/")
(autoload 'imaxima "imaxima" "Maxima with LaTeX output." t)
(autoload 'emaxima "emaxima" "Literate Programming with Maxima." t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    LaTeX                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AUCTeX - Sophisticated document creation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://www.gnu.org/software/auctex/

(require 'latex)
(require 'preview)

(setq TeX-auto-save t
      TeX-save-query nil)
(add-to-list 'TeX-view-program-selection '(output-pdf "xdg-open"))

(defun latex-settings ()
  "LaTeX settings."
  (LaTeX-math-mode t)     ; real-time preview
  (setq TeX-engine 'xetex ; use XeLaTeX default
        preview-colors '((nil nil nil)
                         (1.0 1.0 1.0)
                         (nil nil nil))))
(add-hook 'LaTeX-mode-hook 'latex-settings)


;; RefTeX
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq
  reftex-plug-into-AUCTeX t ; supply arg for macros lick \label, \ref, \cite, \index
  reftex-enable-partial-scans t
  reftex-save-parse-info t
  reftex-use-multiple-selection-buffers t
  reftex-toc-split-windows-horizontally t ;;*toc*buffer在左侧。
  reftex-toc-split-windows-fraction 0.2  ;;*toc*buffer 使用整个frame的比例。
)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)


(provide 'init-progmodes)
;;; init-progmodes.el ends here
