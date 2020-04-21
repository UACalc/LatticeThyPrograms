
; q2x6.fp

;;; This is JB's Q fp lattice for 2 x 6.

(in-package :user)


(setq *lat*
  (let ((gens '(0 1 a b c d e f g h))
        (uc '(
              (0 (a))
              (a (b e))
              (b (c f))
              (c (d g))
              (d (h))
              (e (f))
              (f (g))
              (g (h))
              (h (1))
              (1 ())
           ))
        (meets '(
             ))
        (joins '(
             )))
    (make-fp-lattice gens uc joins meets t ) )
  q2x6 *lat*)

