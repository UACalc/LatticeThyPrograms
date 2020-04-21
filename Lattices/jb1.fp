; jb1.fp


(in-package :user)


(setq *lat*
  (let ((gens '(0 1 a b c d e f))
        (uc '(
              (0 (d e ))
              (e (a b ))
              (d (c b ))
              (b (f))
              (a (1))
              (f (1))
              (c (f))
              (1 ())
           ))
        (meets '(
             ))
        (joins '(
              (1 ((a d)))
              (b ((e d)))
             )))
    (make-fp-lattice gens uc joins meets t ) )
  jb1 *lat*)

