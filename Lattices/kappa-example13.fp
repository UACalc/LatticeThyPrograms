;;; kappa-example13.fp

;;; This is kappa-example.fp modified making p \j q+ < q \j p+.
;;; It is not interesting.

(in-package :user)

(setq *lat*
  (let ((gens '(p p+ q q+ s r))
        (uc '(
              (p+ (p)) 
              (q+ (s q)) 
              (p (s)) 
              (q (r)) 
              (s (r)) 
              (r ()) 
             ))
        (joins '(
              (r ((p+ q) ) )
              (s ((p q+)) )
              ))
        (meets '(
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


