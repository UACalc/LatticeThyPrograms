; willard-dual.fp	6/17/02

;;; This is the dual of the fp lattice associated with 
;;; willard's nullary equation. Also I've switched some roles.

(setq *lat*
  (let ((gens '(a b c d e f))
        (uc '(
	      (a (e f))
	      (b (e))
	      (c (e f))
	      (d (f))
              (e nil)
              (f nil) ))
        (meets nil)
        (joins '(
	      (e ((a b)))
	      (f ((c d))) )) )
    (make-fp-lattice gens uc joins meets t) ) )

