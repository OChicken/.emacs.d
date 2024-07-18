;;; init-crypto.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; A tiny calculator for crypto research 😗
;; Use at your own risk 🙂

;;; Code:

(require 'calc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;            Hamming Weight           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun math-hamming-distance (a b)
  "Calculate the Hamming distance of the input A and B.
Currently only support distance of integers."
  (if (and (Math-integerp a) (Math-integerp b))
      (logcount (logxor a b)) ; logcount is the hamming weight
    (math-reject-arg "Only accept integer arguments." 'integerp)))

(defun math-hamming-mul (a b)
  "Calculate the Hamming product of the input A and B."
  (if (and (Math-integerp a) (Math-integerp b))
      (logand (logcount (logand a b)) 1)
    (math-reject-arg "Only accept integer arguments." 'integerp)))

(defun math-hamming-mul-vm (v mat)
  "Calculate V(1*n)⋅MAT(n*k)."
  (let ((r 0)
	(l (length mat))
	(b 0))
    (dotimes (i l)
      (setq b (nth i mat))
      (setq r (+ r (ash (math-hamming-mul v b) (- (1- l) i)))))
    r))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;                 GF2X                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun math-gf2x-dec2binvec (x &optional b)
  "Convert decimal input X to binary vector B (little endian)."
  (unless (Math-integerp x)
    (math-reject-arg))
  (let ((b (or b '())))
    (if (> x 0)
        (math-gf2x-dec2binvec (ash x -1)
                              (push (logand x 1) b))
      b)))

(defun math-gf2x-deg (f &optional n)
  "Return the degree N of the polynomial F."
  (unless (Math-integerp f)
    (math-reject-arg))
  (let ((n (or n 0)))
    (if (> f 1)
        (math-gf2x-deg (ash f -1) (1+ n))
    n)))

(defun math-gf2x-mod (a m)
  "Calculate poly(A) % poly(M)."
  (unless (and (Math-integerp a) (Math-integerp m))
    (math-reject-arg))
  (let* ((deg-a (math-gf2x-deg a))
         (deg-m (math-gf2x-deg m))
         (deg-a-m (- deg-a deg-m)))
    (if (>= deg-a-m 0)
        (math-gf2x-mod (logxor a (ash m deg-a-m)) m)
      a)))

(defun math-gf2x-mul (a b &optional r)
  "Calculate poly(A) * poly(B) => poly(R) on char(2) finite field."
  (unless (and (Math-integerp a) (Math-integerp b))
    (math-reject-arg))
  (let ((r (or r 0)))
    (if (> b 0)
        (math-gf2x-mul (ash a 1) (ash b -1)
                       (if (= (logand b 1) 1)
                           (logxor r a)
                         r))
      r)))

(defun math-gf2x-mulm (a b m)
  "Calculate poly(A) * poly(B) % poly(M) on char(2) finite field."
  (unless (and (Math-integerp a) (Math-integerp b) (Math-integerp m))
    (math-reject-arg))
  (math-gf2x-mod (math-gf2x-mul a b) m))

(defun math-gf2x-pow (a n &optional r)
  "Calculate pow(poly(A), N) => poly(R) on char(2) finite field."
  (unless (and (Math-integerp a) (Math-integerp n))
    (math-reject-arg))
  (let ((r (or r 1)))
    (if (> n 0)
        (math-gf2x-pow (math-gf2x-mul a a) (/ n 2)
                       (if (= (logand n 1) 1)
                           (math-gf2x-mul r a)
                         r))
      r)))

(defun math-gf2x-pow-mod (a n f)
  "Calculate pow(poly(A), N) % poly(F) on GF(2^deg(F))."
  (let ((r 1))
    (while (> n 0)
      (if (= (logand n 1) 1)
          (setq r (math-gf2x-mod (math-gf2x-mul r a) f)))
      (setq a (math-gf2x-mod (math-gf2x-mul a a) f))
      (setq n (/ n 2)))
    r))

(defun math-gf2x-div-mod (a f)
  "Calculate poly(A) / poly(F) on GF(2^deg(F))."
  (let ((deg-a (math-gf2x-deg a))
        (deg-f (math-gf2x-deg f))
	(d 0) ; deg(a) - deg(f)
        (q 0))
    (while (>= deg-a deg-f)
      (setq d (- deg-a deg-f))
      (setq q (logior q (ash 1 d)))
      (setq a (logxor a (ash f d)))
      (setq deg-a (math-gf2x-deg a)))
    (list q a)))

(defun math-gf2x-ord (a f)
  "Calculate the order of poly(A) on GF(2^def(F))."
  (let ((factors '(3 5 17))  ; factor(255) = (3 5 17)
	(n 1)
	(r 1)
	(o 255))
    (if (or (= a 0) (= a 1))
	(progn
	  (setq o nil)
	  (message "trivial case."))
      (dolist (m factors)   ; m = #subgroups
	(setq n (/ 255 m))  ; n = #(elements in a subgroup)
        (setq r (math-gf2x-pow-mod a n f))
        (if (= r 1)
	    (setq o n))))
    o))

(defun math-gf2x-get-generators (f)
  "Find all generators on GF(2^def(F))."
  (let ((a 255)
        (o 1)
        (r ()))
    (while (>= a 2)
      (setq o (math-gf2x-ord a f))
      (if (= o 255)
          (push a r))
      (setq a (1- a)))
    r))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;                 AES                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar crypto-aes-sbox
  ;    0    1    2    3    4    5    6    7    8    9    a    b    c    d    e    f
  '(#x63 #x7c #x77 #x7b #xf2 #x6b #x6f #xc5 #x30 #x01 #x67 #x2b #xfe #xd7 #xab #x76 ; 0
    #xca #x82 #xc9 #x7d #xfa #x59 #x47 #xf0 #xad #xd4 #xa2 #xaf #x9c #xa4 #x72 #xc0 ; 1
    #xb7 #xfd #x93 #x26 #x36 #x3f #xf7 #xcc #x34 #xa5 #xe5 #xf1 #x71 #xd8 #x31 #x15 ; 2
    #x04 #xc7 #x23 #xc3 #x18 #x96 #x05 #x9a #x07 #x12 #x80 #xe2 #xeb #x27 #xb2 #x75 ; 3
    #x09 #x83 #x2c #x1a #x1b #x6e #x5a #xa0 #x52 #x3b #xd6 #xb3 #x29 #xe3 #x2f #x84 ; 4
    #x53 #xd1 #x00 #xed #x20 #xfc #xb1 #x5b #x6a #xcb #xbe #x39 #x4a #x4c #x58 #xcf ; 5
    #xd0 #xef #xaa #xfb #x43 #x4d #x33 #x85 #x45 #xf9 #x02 #x7f #x50 #x3c #x9f #xa8 ; 6
    #x51 #xa3 #x40 #x8f #x92 #x9d #x38 #xf5 #xbc #xb6 #xda #x21 #x10 #xff #xf3 #xd2 ; 7
    #xcd #x0c #x13 #xec #x5f #x97 #x44 #x17 #xc4 #xa7 #x7e #x3d #x64 #x5d #x19 #x73 ; 8
    #x60 #x81 #x4f #xdc #x22 #x2a #x90 #x88 #x46 #xee #xb8 #x14 #xde #x5e #x0b #xdb ; 9
    #xe0 #x32 #x3a #x0a #x49 #x06 #x24 #x5c #xc2 #xd3 #xac #x62 #x91 #x95 #xe4 #x79 ; a
    #xe7 #xc8 #x37 #x6d #x8d #xd5 #x4e #xa9 #x6c #x56 #xf4 #xea #x65 #x7a #xae #x08 ; b
    #xba #x78 #x25 #x2e #x1c #xa6 #xb4 #xc6 #xe8 #xdd #x74 #x1f #x4b #xbd #x8b #x8a ; c
    #x70 #x3e #xb5 #x66 #x48 #x03 #xf6 #x0e #x61 #x35 #x57 #xb9 #x86 #xc1 #x1d #x9e ; d
    #xe1 #xf8 #x98 #x11 #x69 #xd9 #x8e #x94 #x9b #x1e #x87 #xe9 #xce #x55 #x28 #xdf ; e
    #x8c #xa1 #x89 #x0d #xbf #xe6 #x42 #x68 #x41 #x99 #x2d #x0f #xb0 #x54 #xbb #x16 ; f
    ))

(defvar crypto-aes-exp-table
  ; Pick a generator (here g=3) then calc pow(g,n) for n in (0,255).
  (let ((g (car (math-gf2x-get-generators #x11b)))
	(table '()))
    (dotimes (n 256)
      (push (math-gf2x-pow-mod g n #x11b) table))
    (reverse table)))

(defvar crypto-aes-log-table
  (let ((alist ())
	(i 0))
    (setq alist (mapcar (lambda (x)
			  (prog1
			      (cons i x)
			    (setq i (1+ i))))
			crypto-aes-exp-table))
    (sort alist (lambda (x y) (< (cdr x) (cdr y))))
    (setq alist (mapcar #'car alist))
    (setf (nth 1 alist) 0)
    alist))

(defvar crypto-aes-inv-table
  ; Calc a^{-1} for a in (0,255)
  (let ((l '()))
    (dotimes (a 256)
      (push (math-gf2x-pow-mod a 254 #x11b) l))
    (reverse l)))

(defun crypto-aes-subbytes (x)
  "Do sbox substitution X."
  (interactive)
  (let ((ret (aref crypto-aes-sbox x)))
    ret))

(provide 'init-crypto)
;;; init-crypto.el ends here
