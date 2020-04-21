; fp_proj.fp	12/21/93

;;; This is an attempt to see if there can be exponentially many
;;; meet irreducibles in P^\j (in the input size of $\alg P$).

(in-package :user)

(setq lat4
  (let ((gens '(z a b c d u))
        (uc '(
	      (z (a b c d))
	      (a (u))
	      (b (u))
	      (c (u))
	      (d (u))
	      (u ())))

        (joins '(
		 (u ((a b)))))
        (meets nil) )
    (make-fp-lattice gens uc joins meets ) ) )

(setq lat5
  (let ((gens '(z a b c d e u))
        (uc '(
	      (z (a b c d e))
	      (a (u))
	      (b (u))
	      (c (u))
	      (d (u))
	      (e (u))
	      (u ())))

        (joins '(
		 (u ((a b)))))
        (meets nil) )
    (make-fp-lattice gens uc joins meets ) ) )

(setq *lat*
  (let ((gens '(z a b c d e f u))
        (uc '(
	      (z (a b c d e f))
	      (a (u))
	      (b (u))
	      (c (u))
	      (d (u))
	      (e (u))
	      (f (u))
	      (u ())))

        (joins '(
		 (u ((a b)))))
        (meets nil) )
    (make-fp-lattice gens uc joins meets ) ) )

