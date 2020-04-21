;;; kappa-example12.fp


(in-package :user)

(setq *lat*
  (let ((gens '(p p+ p1 q q+ q1))
        (uc '(
              (p+ (p p1 q)) 
              (q+ (p q1 q)) 
              (p ()) 
              (q ()) 
              (p1 ()) 
              (q1 ()) 
             ))
        (joins '(
              ))
        (meets '(
              (p+ ((p p1) ) )
              (q+ ((q q1) ) )
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


