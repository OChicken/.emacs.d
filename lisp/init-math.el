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

(defun math-identity-matrix-p (mat &optional mul)
  "True if MAT is an identity matrix.  Accept MUL as t to take (0,0) of MAT."
  (if (math-square-matrixp mat)
      (let ((a (if mul
                   (nth 0 (nth 0 mat))
                 1))
            (n (length mat))
            (i 0))
        (while (and (< i n)
                    (math-ident-row-p (nth i mat) i a))
          (setq i (1+ i)))
        (if (= i n)
            t
          nil))))

(defun math-ident-row-p (row n &optional a)
  "Given ROW and N th elm A, predicate its left & right elms are all 0."
  (unless a
    (setq a 1))
  (and
   (not (memq nil (mapcar
                   (lambda (x) (eq x 0))
                   (nthcdr (1+ n) row))))
   (not (memq nil (mapcar
                   (lambda (x) (eq x 0))
                   (butlast
                    (cdr row)
                    (- (length row) n)))))
   (eq (elt row n) a)))

(defun math-mat-idx (A m n)
  "Get the index M,N from matrix A."
  (nth n (nth m A)))

(provide 'init-math)
;;; init-math.el ends here
