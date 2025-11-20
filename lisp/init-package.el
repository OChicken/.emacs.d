;;; init-package.el --- package management and keyring preparation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; package.el --- Simple package system for Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mandatory before installing packages

; (require 'transient)
; "Invalid slot name" issue
; https://emacs.stackexchange.com/a/50781
;; after magit update you don't need that

(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(defun package-install-init (package)
  "Install PACKAGE if not exists."
  (unless (package-installed-p package)
    (package-install package)))

(defun extract-package-name (url)
  "Extract the package name from the given URL."
  (let ((path (url-filename (url-generic-parse-url url))))
    (intern (file-name-nondirectory (file-name-sans-extension path)))))

(defun package-vc-install-init (url)
  "Install PACKAGE if not exists."
  (setq package (extract-package-name url))
  (unless (package-installed-p package)
    (package-vc-install url)))

(unless (package-installed-p 'gnu-elpa-keyring-update)
  ; gnu-elpa-keyring-update need special treatment since without the keyring
  ; you cannot install any packages.
  ; https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure
  (setq package-check-signature nil)
  (package-refresh-contents)
  (package-install 'gnu-elpa-keyring-update)
  (setq package-check-signature 'allow-unsigned) ; set back to default
)

;; init-lang ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (pkg '(cmake-mode
	       cuda-mode
	       crontab-mode ; Major mode for crontab(5) files
	       csv-mode
	       cython-mode
	       dockerfile-mode
	       go-mode
	       gnuplot
	       gnuplot-mode
	       htmlize
	       magma-mode
	       markdown-mode ; Markdown Mode for Emacs
	       pdf-tools ; Emacs support library for PDF files
	       php-mode
	       rust-mode
	       sage-shell-mode
	       tuareg ; OCaml
	       web-mode ; html template editing for emacs
	       yaml
	       yaml-mode))
  (package-install-init pkg))


;; init-view ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (pkg '(diminish ; Diminished modes are minor modes with no modeline display
	       rainbow-mode
	       rainbow-delimiters ; Emacs rainbow delimiters mode
	       mode-line-bell ; Flash the Emacs mode line instead of ringing the bell
	       page-break-lines ; Display ugly ^L page breaks as tidy horizontal lines
	       highlight-escape-sequences ; Highlight escape sequences in Emacs
	       ; vertico ; Vertical Interactive Completion
	       )))

;; init-edit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (pkg '(anzu ; Displays current match and total matches info in the mode-line
	       imenu-list ; Show the current buffer's imenu entries in a separate buffer
	       list-unicode-display ; Search for and list unicode characters in Emacs
	       move-dup ; Moving and duplications of lines or selections
	       multiple-cursors ; Multiple cursors for emacs
	       symbol-overlay ; Highlight symbols with keymap-enabled overlays
	       which-key ; Displays available keybindings in popup
	       wrap-region ; Wrap text with punctation or tag
	       xclip ; Copy&paste GUI clipboard from text terminal
	       clipetty ; Manipulate the system (clip)board with (e)macs from a (tty)
	       vterm ; Emacs libvterm integration
	       yasnippet ; A template system for Emacs
	       yasnippet-snippets ; a collection of yasnippet snippets for many languages
	       wakatime-mode ; Automatic time tracking
	       txl ; Elisp library for the DeepL API
	       sunshine ; Weather forecast plugin, display the forecast from OpenWeatherMap
	       magit ; It's Magit! A Git Porcelain inside Emacs
	       git-gutter ; Emacs port of GitGutter which is Sublime Text Plugin
	       org-cliplink ; Insert org-mode links from clipboard
	       ob-rust
	       ob-go
	       ob-php
	       ob-sagemath
	       ; org-fragtog ; Automatically toggle Org mode LaTeX fragment previews as the cursor enters and exits them
	       ))
  (package-install-init pkg))

(dolist (url '("https://github.com/jkitchin/ox-ipynb" ; org-mode exporter to Jupyter notebooks
	       ))
  (package-vc-install-init url))

;; init-progmodes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (pkg '(projectile ; Project Interaction Library for Emacs
	       ibuffer-vc ; Let Emacs' ibuffer-mode group files by git project etc., and show file state
	       ibuffer-projectile ; Group buffers in Emacs ibuffer-mode by their projectile root directory
	       eat
	       flycheck
               flycheck-clang-tidy
               flycheck-rust
	       company
               company-math
               company-c-headers
               company-auctex
	       copilot
	       gptel
	       slime
	       haskell-mode
	       proof-general
	       ein ; Jupyter notebook client in Emacs
	       auctex ; Sophisticated document creation
	       ))
  (package-install-init pkg))

(dolist (url '("https://github.com/cpoile/claudemacs" ; AI Pair Programming with Claude Code in Emacs
	       ))
  (package-vc-install-init url))

(provide 'init-package)
;;; init-package.el ends here
