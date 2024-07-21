;;; init-crypto.el --- A tiny calculator for math research -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; A tiny calculator for math research 😗
;; Use at your own risk 🙂

;;; Code:

;; (require 'calc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                Matrix               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun math-matrixp (a)
  "True if A is a matrix.  [P x] [Public]."
  (and (vectorp a)
       (vectorp (aref a 0))
       (> (length a) 0)
       (> (length (aref a 0)) 0)
       (let ((len (length (aref a 0))))
         (catch 'not-a-matrix
           (dotimes (i (length a))
             (if (or (not (vectorp (aref a i)))
                     (/= (length (aref a i)) len))
                 (throw 'not-a-matrix nil)))
           t))))

(defun math-mat-idx (A m n)
  "Get the index M,N from matrix A."
  (aref (aref A m) n))

(provide 'init-math)
;;; init-math.el ends here
