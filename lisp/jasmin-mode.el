;;; jasmin-mode.el --- A minimal major mode for the Jasmin DSL -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a minimal major mode for the Jasmin DSL, similar
;; to C for keywords/types highlighting.  It does NOT include advanced
;; compilation or REPL integration.

;;; Code:

(defvar jasmin-mode-hook nil
  "Hook run when entering `jasmin-mode'.")

(setq font-lock-mode t)

(defvar jasmin-keywords
  '("inline" "for" "if" "else" "while" "return" "break" "continue"))

(defvar jasmin-types
  '("int" "float" "double" "char" "void" "bool" "long"))

;; Build regexp，use `regexp-opt` to generate \\(?:\\<keyword\\>\\)
(defvar jasmin-font-lock
  `(
    (,(regexp-opt jasmin-keywords 'words) . font-lock-keyword-face)
    (,(regexp-opt jasmin-types 'words) . font-lock-type-face)
    ; Simply treat all identifiers as variables
    ; Need to further extend to distinguish function names, constants, etc.
    ("\\<[a-zA-Z_][a-zA-Z0-9_]*\\>" . font-lock-variable-name-face)
    ))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jazz\\'" . jasmin-mode))

(define-derived-mode jasmin-mode c-mode "Jasmin"
  "Major mode for editing Jasmin DSL code with basic syntax highlighting."
  ;; 設定語法高亮規則
  (setq font-lock-defaults '((jasmin-font-lock)))
  ;; 強制開啟 font-lock-mode，以確保立即顯示顏色
  (font-lock-mode 1)
  ;; 最後會自動 run `jasmin-mode-hook`，不用手動呼叫
  )

(provide 'jasmin-mode)
;;; jasmin-mode.el ends here
