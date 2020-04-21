; jb.fp		4/29/97


(in-package :user)


(setq *lat*
  (let ((gens '(1 a b c d e ))
        (uc '(
              (a (1))
              (b (c d))
              (c (e))
              (d (e))
              (e (1))
              (1 ())
           ))
        (meets '(
              ;; (0 ((a c)))
              ;;(0 ((a c) (a b) (b c)))
              ))
        (joins '(
              (1 ((a b)))
             )))
    (make-fp-lattice gens uc joins meets nil ) ) )


