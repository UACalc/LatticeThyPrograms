;;; kappa-example3.fp

;;; This is like kappa-example.fp with the dual relations added.
;;; It is finite; with 22 elements.


(in-package :user)

(setq *lat*
  (let ((gens '(z p p+ q q+ r))
        (uc '(
              (z (p+ q+))
              (p+ (p)) 
              (q+ (q)) 
              (p (r)) 
              (q (r)) 
              (r ()) 
             ))
        (joins '(
              (r ((p+ q) (p q+)) )
              ))
        (meets '(
              (z ((p+ q) (p q+)) )
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


