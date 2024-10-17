;;; init-progmodes.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-package)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              Projectile             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Project Interaction Library for Emacs
; https://github.com/bbatsov/projectile
(package-install-init 'projectile)
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
(package-install-init 'ibuffer-vc)

;; Group buffers in Emacs ibuffer-mode by their projectile root directory ;;;;;
; https://github.com/purcell/ibuffer-projectile
(package-install-init 'ibuffer-projectile)
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

(setq-local package-list
            '(flycheck
              flycheck-clang-tidy
              flycheck-rust))
(dolist (package package-list)
  (package-install-init package))

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

(setq-local package-list
            '(company
              company-math
              company-c-headers
              company-auctex))
(dolist (package package-list)
  (package-install-init package))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;          Emacs Lisp config          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (hook '(lisp-mode-hook lisp-interaction-mode-hook))
  (add-hook hook 'display-line-numbers-mode))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

(require 'immortal-scratch)
; Respawn the scratch buffer when it's killed
; https://github.com/jpkotta/immortal-scratch
(add-hook 'after-init-hook 'immortal-scratch-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;             Lisp config             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-install-init 'slime)

(add-hook 'lisp-mode-hook
          (lambda ()
            (unless (featurep 'slime)
              (require 'slime)
              (normal-mode))
            (setq indent-tabs-mode nil)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;             C/C++ config            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cc-mode)
(add-hook 'c-mode-hook
          (lambda ()
            (setq flycheck-gcc-language-standard   "c11"
                  flycheck-clang-language-standard "c11"
                  flycheck-clang-include-path '("~/.local/include/"
                                                "/usr/share/verilator/include/"
                                                ))))

(defun c-format-linux ()
  "Format the current buffer with clang-format using the specified style file."
  (interactive)
  (when (and buffer-file-name (string-match "\\.c\\'" buffer-file-name))
    ;; Save the buffer if it's modified
    (when (buffer-modified-p)
      (save-buffer))
    ;; Call clang-format with the specified style
    (let* ((style-file (expand-file-name "~/.emacs.d/.clang-format"))
           (current-file (buffer-name))
           (style-arg (concat "-style=file:" style-file)))
      (call-process "clang-format" nil nil nil style-arg "-i" current-file))
    (revert-buffer t t t)))
(define-key c-mode-map (kbd "C-c C-f") 'c-format-linux)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;            Python config            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'python)
; Python's flying circus support for Emacs
; file:///usr/share/emacs/29.1/lisp/progmodes/python.el.gz
(setq python-indent-offset 4)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;             LaTeX config            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; AUCTeX - Sophisticated document creation
; https://www.gnu.org/software/auctex/
(package-install-init 'auctex)
(require 'latex)    ; ~/.emacs.d/elpa/auctex-13.2.1/latex.el
(require 'preview)  ; ~/.emacs.d/elpa/auctex-13.2.1/preview.el
(with-eval-after-load 'latex
  (prettify-symbols-mode t)
  (outline-minor-mode t)
  (add-to-list 'TeX-view-program-selection '(output-pdf "qpdfview"))
  (add-to-list 'TeX-view-program-list '("qpdfview" "qpdfview --unique  %o"))
  (setq
    LaTeX-math-mode 1          ; real-time preview
    TeX-auto-save t
    TeX-engine 'xetex          ; use XeLaTeX default
    TeX-show-compilation nil   ; NOT display compilation windows
    TeX-save-query nil
    preview-colors '((nil  nil  nil)
                     (1.0  1.0  1.0)
                     (nil  nil  nil))
    ; preview-pdf-color-adjust-method
))

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
