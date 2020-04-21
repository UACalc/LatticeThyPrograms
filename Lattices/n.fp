;;; n.fp



;(in-package :user)

(setq *lat*
  (let ((gens '(a b c d ))
        (uc '(
              (a (c)) 
              (b (c d)) 
              (c ()) 
              (d ()) 
             )) )
    (make-fp-lattice gens uc nil nil t) ) )


