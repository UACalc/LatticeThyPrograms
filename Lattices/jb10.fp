; jb10.fp

;;; this is really just FL(1 + 3) and so is finite.
;;;
;;;
;;;                              1
;;;                             | \
;;;                             |  \
;;;                             |   \
;;;                             |    \
;;;                             |     o e
;;;                             |      \
;;;                             |       o d
;;;                             |      /
;;;                             |     o c
;;;                             |    / 
;;;                           a o   o b 


(in-package :user)


(setq *lat*
  (let ((gens '(a b c d e 1))
        (uc '(
              (d (e))
              (b (c))
              (a (1))
              (c (d))
              (e (1))
              (1 ())
           ))
        (meets '(
             ))
        (joins '(
              (1 ((a b)))
             )))
    (make-fp-lattice gens uc joins meets t ) )
  jb10 *lat*)

