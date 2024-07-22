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

(defun math-matrixp (a)                                         ; [P x] [Public]
  "True if A is a matrix."
  (and (listp a)
       (listp (nth 0 a))
       (cdr (nth 0 a))
       (let ((len (length (nth 0 a))))
	 (setq a (cdr a))
	 (while (and (setq a (cdr a))
		     (listp (car a))
		     (= (length (car a)) len)))
	 (null a))))

(defun math-mat-dim (m)                                         ; [l x] [Public]
  "Return the dimensions of a matrix M as a list."
  (if (listp m)
      (if (math-matrixp m)
	  (cons (length m)
		(math-mat-dim (nth 0 m)))
	(list (length m)))
    nil))

(defun math-square-matrixp (a)                                  ; [P V] [Public]
  "True if A is a square matrix."
  (let ((dim (math-mat-dim a)))
    (and (cdr dim)
	 (= (car dim) (nth 1 dim)))))


(defun math-mat-idx (A m n)
  "Get the index M,N from matrix A."
  (aref (aref A m) n))

(provide 'init-math)
;;; init-math.el ends here
