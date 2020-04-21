; n5plus2.fp
;;; this is m3 as a fp lattice.

(in-package :user)


(setq *lat*
  (let ((gens '(0 1 a b c ))
        (uc '(
              (0 (a b c))
              (a (1))
              (b (1))
	      (c (1))
              (1 ())
           ))
        (meets '(
              (0 ((a b) (a c) (b c) ))
             ))
        (joins '(
              (1 ((a b) (a c) (b c) ))
             )))
    (make-fp-lattice gens uc joins meets t ) ) )


