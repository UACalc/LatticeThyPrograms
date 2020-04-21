; test15.fp

;;; This satisfies my conditions for weak atomicity but is not. The
;;; quotient a(b+c)/0 seems to have no covering.  If we let  u = a(b+c)
;;; and  v = b,  then  J' = J_0 = {a, b, c}. Closing under joins
;;; gives the 8 element boolean algebra.  The doubling doubles the
;;; interval  b/0  and  b  is mapped to the lower "b".  This is still
;;; 3 generated but does NOT satisfy the defining relations (ac =/ 0)
;;; and so is not an image of  FL(P).

(setq test15
  (make-fp-lattice
    '(a b c 0 1)
    '(
      (a (1))
      (b (1))
      (c (1))
      (1 ())
      (0 (a b c)) )
    nil
    '(
      (0 ((a b) (a c) (b c))) ) ))

(setq *lat* test15)
