;;; kappa-example11.fp

;;; This is just M_3 with a new element between 0 and 1

(in-package :user)

(setq *lat*
  (let ((gens '(a b c r 0 1))
        (uc '(
              (0 (a b c r)) 
              (a (1)) 
              (b (1)) 
              (c (1)) 
              (1 ()) 
              (r (1)) 
             ))
        (joins '(
              (1 ((a c) (a b) (b c) ) )
              ))
        (meets '(
              (0 ((a c) (a b) (b c) ) )
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


