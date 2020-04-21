;;; k.fp

(in-package :user)

(setq *lat*
  (let ((gens '(p q r s a v))
        (uc '(
              (v (p q r s a ))
              (p ()) 
              (q ()) 
              (r ()) 
              (s ()) 
              (a ()) 
             ))
        (joins '(
              ))
        (meets '(
               (v ((r s p)  (a r q)) )
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


