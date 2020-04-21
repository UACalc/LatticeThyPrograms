; file four-atoms-3gen.fp

; Goal find a 3-generated lattice with at least 4 atoms.
;
; The idea is to take 4 elements w_i in FL(x,y,z) and let
; u = \Join (w_i \met w_j) and take the fp lattice with
; the relation u = 0. 
; 
; Right now we are trying the quadruple x, y, z, w,
; for some w.
; 
; We have tried some simple examples which have failed.
; 
; Latest try:
; 
; w = (x + z(x + y))(y + x(y + z))(z + x(y + z)) 
;        + (x + z(x + y))(y + z(x + y))(z + x(y + z))
; 
; (This is the dual of a4 on page 201 of the free lattices book.)
; 
 
 
(in-package :user)

(defun mid4 (w0 w1 w2 w3 &optional (lat *lat*))
  (setq w01 (meet (list w0 w1) lat))
  (setq w02 (meet (list w0 w2) lat))
  (setq w03 (meet (list w0 w3) lat))
  (setq w12 (meet (list w1 w2) lat))
  (setq w13 (meet (list w1 w3) lat))
  (setq w23 (meet (list w2 w3) lat))
  (setq u (join (list w01 w02 w03 w12 w13 w23) lat)) 
  (not (or (lssql w0 u) (lssql w1 u) (lssql w2 u) (lssql w3 u)))
)

(defun test4 (elems k)
  (mid4 (nth k elems) (nth (+ k 1) elems)
        (nth (+ k 2) elems) (nth (+ k 3) elems)))

; since make-fp-lattice-from-relations is not fully implemented,
; this won't work.
(defun mid4-lattice (w0 w1 w2 w3 &optional (lat *lat*))
  (setq gens '(x y z))                          ;;; hard wired !!
  (setq w01 (meet (list w0 w1) lat))
  (setq w02 (meet (list w0 w2) lat))
  (setq w03 (meet (list w0 w3) lat))
  (setq w12 (meet (list w1 w2) lat))
  (setq w13 (meet (list w1 w3) lat))
  (setq w23 (meet (list w2 w3) lat))
  (setq zero (meet gens lat))
  (setq u (join (list w01 w02 w03 w12 w13 w23) lat))
  (setq rels (list (list u zero)))
  (make-fp-lattice-from-relations gens rels)
)



(setq *lat* (make-free-lattice '(x y z)))

; weaving and embedding FL(n) into FL(3)
(defun f (u &optional (lat *lat*)) 
  (join (list 'x (meet (list 'z (join (list 'y (meet (list 'x 
     (join (list 'z (meet (list 'y u) lat)) lat)) lat)) lat)) lat)) lat) )

(defun f-iter (u n &optional (lat *lat*))
  (cond
    ((eq n 0) u lat)
    ((eq n 1) (f u lat))
    (t (f (f-iter u (- n 1) lat) lat) )))

; these are the free generators of FL(n) page 23 of the book
(defun w (n &optional (lat *lat*))
  (let ((tmp (f-iter 'x n lat)))
    (join (list 'z (meet (list tmp (join 
              (list 'y (dual tmp)) lat)) lat)) lat) ))

;; changing variable (a quick hack)
;(setq w1-nums (subst 3 'z (subst 2 'y (subst 1 'x (w 1)




(setq x* (p 'x (s 'y 'z)))
(setq y* (p 'y (s 'x 'z)))
(setq z* (p 'z (s 'x 'y)))
(setq w-common (p (s 'x z*) (s 'z x*)))
(setq w (s (p w-common (s 'y x*)) (p w-common (s 'y z*))))




; The four are x, y, z, w = (x + z(x + y))(y + x(y + z))(z + x(y + z))
;
; Let u = x(y + z) + z(y + x(y + z)) + y(x + z(x + y))(z + x(y + z))
;
; The relation is u = 0.
;
; Alternately 3 relations:
; x_* = 0
; z(y + x(y + z)) = 0
; y(x + z(x + y))(z + x(y + z)) = 0
;
; This can't work: x_* = 0 implies w \le y




;(let* ((gens '(zero x y z a b c d x* y* z* x0 y0 z0 ))
;       (uc '(
;           (x* () )
;           (y* () )
;           (z* () )
;           (x (x*) )
;           (y (y*) )
;           (z (z*) )
;           (x0 (x d))
;           (y0 (y d))
;           (z0 (z d))
;           (d (x* y* z*))
;           (a (y z d))
;           (b (x z d))
;           (c (x y d))
;	   (zero (a b c))))
;       (joins '(
;              (x* ((x a)))
;              (y* ((y b)))
;              (z* ((z c)))
;))
;       (meets '(
;              (a ((y z)))
;              (b ((x z)))
;              (c ((x y)))
;              (d ((x* y* z*)))
;              (zero ((x d)))
;              (zero ((y d)))
;              (zero ((z d)))
;              (zero ((x y z))))))
;      (setq four-atoms (make-fp-lattice gens uc joins meets t)))
;
;(setq *lat* four-atoms)






