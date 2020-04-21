; willard.fp	9/11/91

;;; This is the fp lattice associated with willard's nullary equation.

(setq *lat*
  (let ((gens '(a b c d e f))
        (uc '(
	      (a nil)
	      (b nil)
	      (c nil)
	      (d nil)
              (e (a b c))
              (f (a b d)) ))
        (joins nil)
        (meets '(
	      (e ((a c)))
	      (f ((b d))) )) )
    (make-fp-lattice gens uc joins meets t) ) )

