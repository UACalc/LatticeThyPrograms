; one2.fp


;;; This can be defined with one relation: a \meet b <= c.


(in-package :user)


(setq *lat*
  (let ((gens '(a b c 0))
        (uc '(
              (0 (a b c))
              (b ())
              (a ())
              (c ())
           ))
        (meets '(
              (0 ((a b) ))
             ))
        (joins '(
             )))
    (make-fp-lattice gens uc joins meets t ) )
  one2 *lat*)

