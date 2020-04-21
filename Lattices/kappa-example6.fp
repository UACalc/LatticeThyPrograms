;;; kappa-example6.fp

;;; This is an attempt to make the w more complex.

;;; In this example \kappa(p) = \kappa(q); both have the single element
;;; w, where w is more complicated than in kappa-example.fp. 
;;; Thus \kappa^\dual(w) is \{p, q\}. 
;;; w = p_\dagger \j q_\dagger \j ((c \j d \j p) \m (c \j d \j q))

;(in-package :user)

(setq *lat*
  (let ((gens '(c d e p p+ q q+ r))
        (uc '(
              (p+ (p)) 
              (q+ (q)) 
              (p (r)) 
              (q (r)) 
              (e (c d p)) 
              (r ()) 
              (c ()) 
              (d ()) 
             ))
        (joins '(
              (r ((p+ q) (p q+)) )
              ))
        (meets '(
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


