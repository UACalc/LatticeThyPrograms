;;; nearM3-2.fp

;;; Testing how soon you find ji, not cji elements in an infinite fp lattice.

;(in-package :user)

(setq *lat*
  (let ((gens '(0 1 a b c p))
        (uc '(
              (0 (p b c))
              (p (a))
              (a (1))
              (b (1))
              (c (1))
              (1 ()) ))
        (joins '(
              (1 ((a b) (a c) (b c))) 
              ))
        (meets '(
               (0 ((a b) (a c) (b c))) 
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


