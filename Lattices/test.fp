
(in-package :user)

(setq *lat*
  (let ((gens '(a b c d p q))
        (uc '(
              (d (q))
              (a (p q))
              (b (p))
              (c (p)) 
	      (p ())
	      (q ())
	      ))
        (joins '(
              (q ((a d)))
              (p ((a b) (b c))) ))
        (meets nil) )
    (make-fp-lattice gens uc joins meets ) ) )


