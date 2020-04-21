; shushpanov1.fp
;;; 2020/01/01 
;;; The order of this partial lattice is that of FD(a,b,c)
;;; with the two medians separted. So there are 19 elements.
;;; This one has only the "free" relations so a, b, c
;;; generate FL(a,b,c).
;;; shushpanov2.fp will have 4 extra relations
;;;
;;;    a + bc = (a+b)(a+c)
;;;    b + ac = (a+b)(b+c)
;;;    c + ab = (b+c)(a+c)
;;;    a(b + c) = ab + ac
;;;
;;; That is a is both distributive and duall distributive.
;;; b and c are both distributive. (This uses a result of Shushpanov.
;;; Shushpanov proved this lattice is infinite but asked if it
;;; contains FL(3) as a sublattice.


;(in-package :user)

(setq *lat*
  (let ((gens '(a b c a0 b0 c0 a1 b1 c1 d e f g h i m0 m1 u z))
        (uc '(
              (a (a1))
              (b (b1))
              (c (c1))
              (d (b0 c0))
              (e (a0 c0))
              (f (a0 b0))
              (z (d e f))
              (a0 (a m0))
              (b0 (b m0))
              (c0 (c m0))
              (m0 (m1))
              (m1 (a1 b1 c1))
              (a1 (h i))
              (b1 (g i))
              (c1 (g h))
              (g (u))
              (h (u))
              (i (u))
              (u ())
           ))
        (meets '(
              (d ( (b c)))
              (e ( (a c)))
              (f ( (a b)))
              (z ( (a b c)))
              (m1 ( (g h i)))
              (a1 ((h i))) 
              (b1 ((g i))) 
              (c1 ((g h))) 
           ))
        (joins '(
              (a0 ((e f)))
              (b0 ((d f)))
              (c0 ((d e)))
              (m0 ((d e f)))
              (g ((b c)))
              (h ((a c)))
              (i ((a b)))
              (u ((a b c)))
           )))
    (make-fp-lattice gens uc joins meets nil ) ) )


