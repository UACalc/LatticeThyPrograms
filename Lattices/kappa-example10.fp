;;; kappa-example10.fp

;;; This is just the free product of M_3 and a single element.

(in-package :user)

(setq *lat*
  (let ((gens '(a b c r 0 1))
        (uc '(
              (0 (a b c)) 
              (a (1)) 
              (b (1)) 
              (c (1)) 
              (1 ()) 
              (r ()) 
             ))
        (joins '(
              (1 ((a c) (a b) (b c) ) )
              ))
        (meets '(
              (0 ((a c) (a b) (b c) ) )
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


