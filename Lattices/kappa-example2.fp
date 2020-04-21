;;; kappa-example2.fp

;;; In this example \kappa(p) = \kappa(q); both have the single element
;;; w, where w is more complicated than in kappa-example.fp. 
;;; Thus \kappa^\dual(w) is \{p, q\}. 

(in-package :user)

(setq *lat*
  (let ((gens '(a c d e p p+ q q+ r))
        (uc '(
              (p+ (p)) 
              (q+ (q)) 
              (p (r a)) 
              (q (r)) 
              (r ()) 
              (e (a c d)) 
              (c ()) 
              (d ()) 
              (a ()) 
             ))
        (joins '(
              (r ((p+ q) (p q+)) )
              ))
        (meets '(
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


