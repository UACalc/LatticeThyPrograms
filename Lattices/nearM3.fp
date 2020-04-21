;;; nearM3.fp

(in-package :user)

(setq *lat*
  (let ((gens '(0 1 a b c))
        (uc '(
              (0 (a b c))
              (a (1))
              (b (1))
              (c (1))
              (1 ()) ))
        (joins '(
              (1 ((a b) (a c) (b c))) 
              ;; (1 ( (b c))) 
              ;; (1 ((a b) (a c) )) 
              ))
        (meets '(
               (0 ((a b) (a c) (b c))) 
               ;;(0 ((a b)  (b c))) 
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


