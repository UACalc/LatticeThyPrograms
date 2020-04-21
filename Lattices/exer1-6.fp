
(setq *lat*
  (let ((gens '(a b c))
        (uc '(
              (a ())
              (b (c))
              (c ()) 
	      ))
        (joins nil)
        (meets nil) )
    (make-fp-lattice gens uc joins meets ) ) )


