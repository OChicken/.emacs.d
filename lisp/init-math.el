;;; init-crypto.el --- A tiny calculator for math research -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; A tiny calculator for math research 😗
;; Use at your own risk 🙂

;;; Code:

(require 'calc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                Matrix               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun math-mat-idx (A m n)
  "Get the index M,N from matrix A."
  (nth n (nth m A)))

(provide 'init-math)
;;; init-math.el ends here
