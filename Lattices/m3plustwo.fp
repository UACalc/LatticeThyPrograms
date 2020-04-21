; m3plustwo.fp
;;; this is m3 with a p and q added

(in-package :user)


(setq *lat*
  (let ((gens '(0 1 a b c p q))
        (uc '(
              (0 (p b c))
	      (p (a q))
              (a (1))
              (b (1))
	      (c (q))
	      (q (1))
              (1 ())
           ))
        (meets '(
              (0 ((a b) (a c) (b c) ))
              (p ((a q) ))
             ))
        (joins '(
              (1 ((a b) (a c) (b c) ))
              (q ((c p) ))
             )))
    (make-fp-lattice gens uc joins meets t ) ) )


