;;; kappa-example1.fp

;;; This just makes p_\dagger \join q_\dagger an element of P. The
;;; result is the same: w = c + pq has kappa = {p,q}.

;;; In this example \kappa(p) = \kappa(q); both have the single element
;;; w = p_\dagger \join q_\dagger \join (p \meet q). Thus \kappa^\dual(w)
;;; is \{p, q\}. 

(in-package :user)

(setq *lat*
  (let ((gens '(p p+ q q+ c r))
        (uc '(
              (p+ (p c)) 
              (q+ (q c)) 
              (c (r))
              (p (r)) 
              (q (r)) 
              (r ()) 
             ))
        (joins '(
              (r ((p+ q) (p q+)) )
              (c ((p+ q+) ) )
              ))
        (meets '(
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


