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

(diminish 'hs-minor-mode)

;; flyspell.el --- On-the-fly spell checker ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "aspell")
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))


;; ispell.el --- interface to spell checkers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ispell)
(setq ispell-dictionary "en")


;; Displays current match and total matches info in the mode-line ;;;;;;;;;;;;;
; https://github.com/emacsorphanage/anzu

(package-install-init 'anzu)
(global-anzu-mode +1)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)
(set-face-attribute 'anzu-mode-line nil
		    :foreground "white"
		    :weight 'bold)


;; Show the current buffer's imenu entries in a separate buffer ;;;;;;;;;;;;;;;
; https://github.com/bmag/imenu-list

(package-install-init 'imenu-list)
(require 'imenu-list)
(setq imenu-list-size 50)


;; Search for and list unicode characters in Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/purcell/list-unicode-display
(package-install-init 'list-unicode-display)


;; Moving and duplications of lines or selections with convenient key bindings;
; https://github.com/wyuenho/move-dup

(package-install-init 'move-dup)
; When paredit is enabled (e.g. in org mode), it will use those keybindings
; M-up and M-down. Therefore, you might prefer to use M-S-up and M-S-down,
; which will work even in lisp modes.
(global-set-key (kbd "M-S-<up>")   'move-dup-move-lines-up)
(global-set-key (kbd "M-S-<down>") 'move-dup-move-lines-down)


;; Multiple cursors for emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/magnars/multiple-cursors.el

(package-install-init 'multiple-cursors)
(global-set-key (kbd "C-c C-SPC") 'mc/mark-pop)
(global-set-key (kbd "C-M-l")   'mc/mark-next-like-this)     ; origin: reposition-window
(global-set-key (kbd "C-M-S-l") 'mc/mark-previous-like-this) ; origin: recenter-other-window
(global-set-key (kbd "C-M-w")   'mc/mark-next-like-this-word)      ; origin: append-next-kill
(global-set-key (kbd "C-M-S-w") 'mc/mark-previous-like-this-word)  ; origin: NULL
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)


;; Highlight symbols with keymap-enabled overlays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;; Displays available keybindings in popup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/justbur/emacs-which-key

(package-install-init 'which-key)
(require 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(which-key-setup-side-window-right)
(setq which-key-idle-delay 5
      which-key-popup-type 'side-window)


;; Wrap text with punctation or tag ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;; Copy&paste GUI clipboard from text terminal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/emacsmirror/xclip/tree/master

(when (or (getenv "DISPLAY")
          (getenv "WAYLAND_DISPLAY"))
  (package-install-init 'xclip)
  (xclip-mode t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               isearch               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Highlight symbols with keymap-enabled overlays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;; Displays current match and total matches info in the mode-line ;;;;;;;;;;;;;
; https://github.com/emacsorphanage/anzu

(package-install-init 'anzu)
(global-anzu-mode +1)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)
(set-face-attribute 'anzu-mode-line nil
		    :foreground "white"
		    :weight 'bold)
(diminish 'anzu-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;         VC: version control         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It's Magit! A Git Porcelain inside Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://magit.vc/

(package-install-init 'magit)
(require 'magit)
(setq magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv))


;; Emacs port of GitGutter which is Sublime Text Plugin ;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/emacsorphanage/git-gutter/

(package-install-init 'git-gutter)
(global-git-gutter-mode +1)
; A long lasting bug: git-gutter annotation disappeared during buffer switching.
; https://github.com/emacsorphanage/git-gutter/issues/155
(global-set-key (kbd "C-x v g") 'git-gutter)  ; vc-annotate
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-x v n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-x v p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
(diminish 'git-gutter-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ibuffer: operate on buffers according to git or projectile ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We have two choices when using ibuffer: filter by either .git or project

; Let Emacs' ibuffer-mode group files by git project etc., and show file state
; https://github.com/purcell/ibuffer-vc
(package-install-init 'ibuffer-vc)

; Group buffers in Emacs ibuffer-mode by their projectile root directory
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



;;;;;;;;;;;;;;;;
;; Navigation ;;
;;;;;;;;;;;;;;;;

; Show the current buffer's imenu entries in a separate buffer
; https://github.com/bmag/imenu-list
(package-install-init 'imenu-list)
(require 'imenu-list)
(setq imenu-list-size 50)


(provide 'init-edit)
;;; init-edit.el ends here
