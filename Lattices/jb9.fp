; jb9.fp

;;; this is really just FL(1 + 3) and so is finite.
;;;
;;;
;;;                           d o       o h
;;;                             |\     /
;;;                             | \   o f
;;;                             |  \ / 
;;;                           a o   o b 


(in-package :user)


(setq *lat*
  (let ((gens '(a b d f h))
        (uc '(
              (d ())
              (b (d f))
              (a (d))
              (f (h))
              (h ())
           ))
        (meets '(
             ))
        (joins '(
              (d ((a b)))
             )))
    (make-fp-lattice gens uc joins meets t ) )
  jb9 *lat*)

