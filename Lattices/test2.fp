
(in-package :user)

;;; this dual is in test3.fp

(setq *lat*
  (let ((gens '(p1 p2 p q1 q2 q r))
        (uc '(
              (p1 (p))
              (p2 (p))
              (q1 (p q))
              (q2 (q))
              (p ())
              (q ())
              (r ())
           ))
        (joins '(
              (p ((p1 p2))) 
              (q ((q1 q2))) ))
        (meets nil))
    (make-fp-lattice gens uc joins meets ) ) )


