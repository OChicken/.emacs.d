;;; init-edit.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some basic preferences ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq set-mark-command-repeat-pop t
;;       truncate-partial-width-windows nil)

;; (transient-mark-mode t)

;; anzu -- Displays current match and total matches info in the mode-line ;;;;;
; https://github.com/emacsorphanage/anzu

(global-anzu-mode +1)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)
(set-face-attribute 'anzu-mode-line nil
                    :foreground "white"
                    :weight 'bold)
(diminish 'anzu-mode)


;; imenu-list --- Show the current buffer's imenu entries in a separate buffer
; https://github.com/bmag/imenu-list

(require 'imenu-list)
(setq imenu-list-size 50)


;; move-dup --- Moving and duplications of lines or selections ;;;;;;;;;;;;;;;;
; https://github.com/wyuenho/move-dup

; When paredit is enabled (e.g. in org mode), it will use those keybindings
; M-up and M-down. Therefore, you might prefer to use M-S-up and M-S-down,
; which will work even in lisp modes.
(global-set-key (kbd "M-S-<up>")   'move-dup-move-lines-up)
(global-set-key (kbd "M-S-<down>") 'move-dup-move-lines-down)


;; multiple-cursors --- Multiple cursors for emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/magnars/multiple-cursors.el

(global-set-key (kbd "C-c C-SPC") 'mc/mark-pop)
(global-set-key (kbd "C-M-l")   'mc/mark-next-like-this)     ; origin: reposition-window
(global-set-key (kbd "C-M-S-l") 'mc/mark-previous-like-this) ; origin: recenter-other-window
(global-set-key (kbd "C-M-w")   'mc/mark-next-like-this-word)      ; origin: append-next-kill
(global-set-key (kbd "C-M-S-w") 'mc/mark-previous-like-this-word)  ; origin: NULL
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)


;; symbol-overlay --- Highlight symbols with keymap-enabled overlays ;;;;;;;;;;
; https://github.com/wolray/symbol-overlay

(require 'symbol-overlay)
(dolist (hook '(prog-mode-hook
                text-mode-hook
                html-mode-hook
                yaml-mode-hook
                conf-mode-hook
                eshell-mode-hook
                org-mode-hook))
  (add-hook hook 'symbol-overlay-mode))
(with-eval-after-load 'symbol-overlay
  (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))
(diminish 'symbol-overlay-mode)


;; which-key --- Displays available keybindings in popup ;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/justbur/emacs-which-key

(require 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(which-key-setup-side-window-right)
(setq which-key-idle-delay 5
      which-key-popup-type 'side-window)
(diminish 'which-key-mode)


;; wrap-region --- Wrap text with punctation or tag ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/rejeep/wrap-region.el

(require 'wrap-region)
(wrap-region-global-mode t)
(wrap-region-add-wrappers
 '(("*" "*" nil org-mode)    ; bold
   ("/" "/" nil org-mode)    ; italic
   ("_" "_" nil org-mode)    ; underline
   ("~" "~" nil org-mode)    ; code
   ("=" "=" nil org-mode)    ; verbatim
   ("+" "+" nil org-mode)))  ; strike-through
(diminish 'wrap-region-mode)


;; xclip --- Copy&paste GUI clipboard from text terminal ;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/emacsmirror/xclip/tree/master

(when (or (getenv "DISPLAY")
          (getenv "WAYLAND_DISPLAY"))
  (xclip-mode t))


;; Clipetty --- Manipulate the system (clip)board with (e)macs from a (tty) ;;;
; https://github.com/spudlyo/clipetty
; https://emacs.stackexchange.com/questions/41339/copy-paste-between-ssh-terminal-emacs-and-macos

(require 'clipetty)
(global-clipetty-mode)


;; yasnippet --- A template system for Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/joaotavora/yasnippet

(yas-global-mode 1)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(with-eval-after-load 'yasnippet
  (define-key yas-minor-mode-map (kbd "C-c &") nil))  ; barely used, but C-c & is used by org-mark-ring-goto
(diminish 'yas-minor-mode)


;; Wakatime: Automatic time tracking
;; https://github.com/wakatime/wakatime-mode
(require 'wakatime-mode)
(add-hook 'after-init-hook 'global-wakatime-mode)
(setq wakatime-cli-path (expand-file-name
                        (concat "~/.wakatime/wakatime-cli-linux-"
                                (cond
                                 ((string-match-p "x86_64" system-configuration) "amd64")
                                 ((string-match-p "aarch64\\|arm64" system-configuration) "arm64")
                                 (t "amd64")))) ; fallback to amd64
      wakatime-api-key (getenv "WAKATIME-API-KEY"))
(diminish 'wakatime-mode)


;; Weather forecast plugin, display the forecast from OpenWeatherMap.
; https://github.com/aaronbieber/sunshine.el
(require 'sunshine)
(setq sunshine-location "Lund"
      sunshine-appid (getenv "SUNSHINE-APPID")
      sunshine-show-icons t
      sunshine-units 'metric)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             VC: Version Control                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It's Magit! A Git Porcelain inside Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://magit.vc/

(require 'magit)
(global-unset-key (kbd "C-x g")) ; magit-status (use "magit" to open it instead)
(setq magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv))
(global-set-key (kbd "C-x g m") 'magit)


;; Emacs port of GitGutter which is Sublime Text Plugin ;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/emacsorphanage/git-gutter/

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


; Agenda ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(setq org-default-notes-file "~/gtd.org")
(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* TODO %?\nCLOCK: %U\n " :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\nCLOCK: %a\n " :clock-resume t)
        ))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(setq org-agenda-files (list "~/gtd.org")
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

; Insert org-mode links from clipboard
; https://github.com/rexim/org-cliplink
(require 'org-cliplink)
(global-set-key (kbd "C-c y") 'org-cliplink)
(setq org-cliplink-max-length 120)  ; cuts any title that exceeds the limit


; Org Refile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


; Babel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ob)
; Working with Code Blocks in Org
; file:///usr/share/emacs/29.1/lisp/org/ob.el.gz

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((coq . t)
      (calc . t)
      (ditaa . t)
      (gnuplot . t)
      (go . t)
      (js .t)
      (lisp . t)
      (php . t)
      (sagemath . t)
      (sql . t)))))

(require 'ob-latex)
; Babel Functions for LaTeX
; file:///usr/share/emacs/29.1/lisp/org/ob-latex.el.gz
; (setq org-babel-latex-pdf-svg-process "dvisvgm --pdf %f -o %O")

(require 'ob-ditaa)
(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

(require 'ob-sagemath)
; org-babel integration with SageMath
; https://github.com/sagemath/ob-sagemath
(set-variable 'sage-shell:use-prompt-toolkit nil)  ; for Ipython >=7 (sage-shell-mode.el) (same below)
(set-variable 'sage-shell:use-simple-prompt  t)
(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)


; Export ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ox-ipynb)

(file-name-directory "/home/OChicken/.emacs.d/lisp/init-edit.el")

(defun org-to-ipynb ()
  "Convert INFILE so that jupyter-python blocks work with ox-ipynb/emacs-jupyter.
Writes a sibling file named <stem>.ipynb."
  (interactive)
  (let* ((abs  (buffer-file-name))
         (dir  (file-name-directory abs))
         (base (file-name-nondirectory abs))
         (stem (file-name-sans-extension base))
         (out-org (expand-file-name (format "%s-jupyter.org" stem) dir))
         (temp-ipynb (expand-file-name (format "%s-jupyter.ipynb" stem) dir))
         (final-ipynb (expand-file-name (format "%s.ipynb" stem) dir)))
    (with-temp-buffer
      ;; read
      (insert-file-contents abs)
      ; 1. begin_src python --> begin_src jupyter-python
      (let ((case-fold-search t)) ; case-insensitive like sed -I
        (goto-char (point-min))
        (while (re-search-forward
                "^[[:space:]]*#\\+begin_src[[:space:]]+python\\(?:[[:space:]].*\\)?$"
                nil t)
          (replace-match "#+begin_src jupyter-python" t t)))
      ; 2. Two-line RESULTS-Block with only one [[file:...]]-Link → only the Link
      (goto-char (point-min))
      (while (re-search-forward
              (concat
               "^[[:space:]]*#\\+RESULTS:[[:space:]]*\\n"
               "[[:space:]]*\\(\\[\\[file:[^]\n]+\\]\\][[:space:]]*\\)$")
              nil t)
        (replace-match "\\1" t))
      ;; write
      (write-region (point-min) (point-max) out-org nil 'silent))
    (ox-ipynb-export-org-file-to-ipynb-file out-org)
    (delete-file out-org)
    ;; Rename the output file from <stem>-jupyter.ipynb to <stem>.ipynb
    (when (file-exists-p temp-ipynb)
      (rename-file temp-ipynb final-ipynb t)
      (message "Wrote %s" final-ipynb))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        txl.el --- Provides machine translation via DeepL's REST API        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; URL: https://github.com/tmalsburg/txl.el

(require 'txl)

(defcustom txl-deepl-glossary-id nil
  "DeepL glossary ID to use for translations.
Set this to your glossary ID, e.g., \"8b9f2c14-cf2b-424f-958c-60d98f07db75\"."
  :type '(choice (const :tag "No glossary" nil)
                 (string :tag "Glossary ID"))
  :group 'txl)

(setq txl-deepl-api-key (getenv "DEEPL-API-KEY"))
(setq txl-languages '(DE . EN-US))
(setq txl-deepl-glossary-id (getenv "DEEPL-API-GLOSSARY"))

(declare-function unfill-region "init.el")
(declare-function replace-asian-punctuation "init.el")
(defun txl-translate-pdf-paragraph (start end)
  "Translate a paragraph that copied from pdf, with text between START & END."
  (interactive "r")
  (save-excursion
    (unfill-region start end)
    ;; Explicitly set the region for txl-translate-region-or-paragraph
    (goto-char start)
    (set-mark end)
    (activate-mark)
    (txl-translate-region-or-paragraph)
    (txl-accept-translation)
    ;; After translation, point is at the end of replaced text
    (let ((new-end (point)))
      ;; Go back to find the start of the paragraph
      (backward-paragraph)
      (replace-asian-punctuation (point) new-end))))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c t") 'txl-translate-pdf-paragraph))

;; DeepL Glossary Support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun txl-glossary-list ()
  "List all DeepL glossaries and display their IDs."
  (interactive)
  (let* ((request-backend 'url-retrieve)
         (api-url (if (string-match-p "api-free" txl-deepl-api-url)
                      "https://api-free.deepl.com/v3/glossaries"
                    "https://api.deepl.com/v3/glossaries"))
         (response (request
                     api-url
                     :type "GET"
                     :sync t
                     :headers `(("Authorization" . ,(concat "DeepL-Auth-Key " txl-deepl-api-key)))
                     :parser 'json-read)))
    (pcase (request-response-status-code response)
      (200
       (let* ((data (request-response-data response))
              (glossaries (cdr (assoc 'glossaries data))))
         (if (= (length glossaries) 0)
             (message "No glossaries found")
           (with-current-buffer (get-buffer-create "*DeepL Glossaries*")
             (erase-buffer)
             (insert "DeepL Glossaries:\n\n")
             (dotimes (i (length glossaries))
               (let* ((glossary (aref glossaries i))
                      (id (cdr (assoc 'glossary_id glossary)))
                      (name (cdr (assoc 'name glossary)))
                      (dicts (cdr (assoc 'dictionaries glossary))))
                 (insert (format "%d. %s\n" (1+ i) name))
                 (insert (format "   ID: %s\n" id))
                 (dotimes (j (length dicts))
                   (let* ((dict (aref dicts j))
                          (src (cdr (assoc 'source_lang dict)))
                          (tgt (cdr (assoc 'target_lang dict)))
                          (cnt (cdr (assoc 'entry_count dict))))
                     (insert (format "   - %s → %s (%d entries)\n" src tgt cnt))))
                 (insert "\n")))
             (goto-char (point-min))
             (display-buffer (current-buffer))))
         glossaries))
      (_ (error "Failed to list glossaries")))))

(defun txl-glossary-get-entries (&optional glossary-id source-lang target-lang)
  "Retrieve and display entries from a DeepL glossary.
If GLOSSARY-ID is not provided, uses `txl-deepl-glossary-id'.
If SOURCE-LANG and TARGET-LANG are not provided, prompts for them."
  (interactive
   (list nil
         (read-string "Source language code (e.g., en, zh, de): " "en")
         (read-string "Target language code (e.g., zh, de, en): " "zh")))
  (unless glossary-id
    (setq glossary-id txl-deepl-glossary-id))
  (unless glossary-id
    (error "No glossary ID specified"))
  (let* ((source-lang (downcase source-lang))
         (target-lang (downcase target-lang))
         (request-backend 'url-retrieve)
         (base-url (if (string-match-p "api-free" txl-deepl-api-url)
                       (format "https://api-free.deepl.com/v3/glossaries/%s/entries" glossary-id)
                     (format "https://api.deepl.com/v3/glossaries/%s/entries" glossary-id)))
         (api-url (format "%s?source_lang=%s&target_lang=%s" base-url source-lang target-lang))
         (response (request
                     api-url
                     :type "GET"
                     :sync t
                     :headers `(("Authorization" . ,(concat "DeepL-Auth-Key " txl-deepl-api-key))
                                ("Accept" . "text/tab-separated-values"))
                     :parser 'buffer-string)))
    (pcase (request-response-status-code response)
      (200
       (let ((entries (request-response-data response)))
         (with-current-buffer (get-buffer-create "*DeepL Glossary Entries*")
           (erase-buffer)
           (insert (format "Glossary ID: %s\n\n" glossary-id))
           (insert "Entries (TSV format):\n")
           (insert "---\n")
           (insert entries)
           (goto-char (point-min))
           (display-buffer (current-buffer)))
         (message "Retrieved entries from glossary")
         entries))
      (404 (error "Glossary not found: %s" glossary-id))
      (_ (error "Failed to retrieve entries. Status: %s"
                (request-response-status-code response))))))

;; Override txl-translate-string to support glossaries and source_lang
(defun txl-translate-string-with-glossary (text target-lang &rest more-target-langs)
  "Translate TEXT to TARGET-LANG with glossary support.
If `txl-deepl-glossary-id' is set, uses that glossary.
If MORE-TARGET-LANGS is non-nil, translation will be applied recursively."
  (message "Requesting translation to %s%s..."
           target-lang
           (if txl-deepl-glossary-id " (with glossary)" ""))
  (let* ((request-backend 'url-retrieve)
         ;; Determine source language
         (source-lang (if (eq target-lang (car txl-languages))
                          (cdr txl-languages)
                        (car txl-languages)))
         ;; Normalize source_lang (remove region code)
         (source-lang-normalized (downcase (car (split-string (symbol-name source-lang) "-"))))
         ;; Build request data
         (request-data `(("auth_key"            . ,txl-deepl-api-key)
                         ("split_sentences"     . ,(pcase txl-deepl-split-sentences
                                                     ((pred not) "0")
                                                     ('nonewlines "nonewlines")
                                                     ((pred (lambda (x) (eq t x))) "1")))
                         ("preserve_formatting" . ,(if txl-deepl-preserve-formatting "1" "0"))
                         ("formality"           . ,(symbol-name txl-deepl-formality))
                         ("text"                . ,text)
                         ("source_lang"         . ,source-lang-normalized)
                         ("target_lang"         . ,(downcase (symbol-name target-lang)))))
         ;; Add glossary_id if configured
         (request-data (if txl-deepl-glossary-id
                           (append request-data `(("glossary_id" . ,txl-deepl-glossary-id)))
                         request-data))
         (response (request
                     txl-deepl-api-url
                     :type "POST"
                     :sync t
                     :parser 'json-read
                     :data request-data)))
    (pcase (request-response-status-code response)
      (200
       (let* ((data (request-response-data response))
              (translations (cdr (assoc 'translations data)))
              (translation (cdr (assoc 'text (aref translations 0))))
              (translation (decode-coding-string (encode-coding-string translation 'latin-1) 'utf-8)))
         (if more-target-langs
             (apply #'txl-translate-string-with-glossary translation (car more-target-langs) (cdr more-target-langs))
           translation)))
      (400 (error "Bad request. Please check error message and your parameters"))
      (403 (error "Authorization failed. Please supply a valid auth_key parameter"))
      (404 (error "The requested resource could not be found"))
      (413 (error "The request size exceeds the limit"))
      (429 (error "Too many requests. Please wait and resend your request"))
      (456 (error "Quota exceeded. The character limit has been reached"))
      (503 (error "Resource currently unavailable. Try again later"))
      (_   (error "Internal error")))))

;; Advice to use glossary-enabled version
(advice-add 'txl-translate-string :override #'txl-translate-string-with-glossary)

(provide 'init-edit)
;;; init-edit.el ends here
