;;; kappa-example.fp

;;; In this example \kappa(p) = \kappa(q); both have the single element
;;; w = p_\dagger \join q_\dagger \join (p \meet q). Thus \kappa^\dual(w)
;;; is \{p, q\}. 

(in-package :user)

(setq *lat*
  (let ((gens '(p p+ q q+ r))
        (uc '(
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
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


