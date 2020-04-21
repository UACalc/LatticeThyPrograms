; n5plus2.fp
;;; this is m3 as a fp lattice.


(setq *lat*
  (let ((gens '(z u a b c ))
        (uc '(
              (z (a b c))
              (a (u))
              (b (u))
	      (c (u))
              (u ())
           ))
        (meets '(
              (z ((a b) (a c) (b c) ))
             ))
        (joins '(
              (u ((a b) (a c)  ))
             )))
    (make-fp-lattice gens uc joins meets t ) ) )


