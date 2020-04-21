;;; kappa-example9.fp

;;; This is just M_3 with a \j c = 1 and a \m c = 0. b is not cji.

(in-package :user)

(setq *lat*
  (let ((gens '(a b c 0 1))
        (uc '(
              (0 (a b c)) 
              (a (1)) 
              (b (1)) 
              (c (1)) 
              (1 ()) 
             ))
        (joins '(
              (1 ((a c) ) )
              ))
        (meets '(
              (0 ((a c) ) )
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


