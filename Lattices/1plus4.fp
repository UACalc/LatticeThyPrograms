;;; 1plus4.fp   5/23/97	Ralph Freese

;;; Rolf's lattice FL(1+4).
;;; The lattice has exactly one join irreducible element which not cji:
;;;
;;; d(a + b)(c + ae)
;;;
;;; Note this shows my conjecture, that FL(P) is finite iff P^{\j\m}
;;; has no ji which is not cji and dually, is false.
;;;
;;;


(in-package :user)

(setq twoplustwo
  (make-free-lattice
    '(a b c d e)
    '(
      (a ())
      (b (c))
      (c (d))
      (d (e))
      (e ())
     ) ) )

(setq *lat* twoplustwo)

