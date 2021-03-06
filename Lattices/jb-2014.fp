;;; d2-like.fp

;;; The lattice generated by a0, a1, b0, b1, 1, m with 
;;;
;;;     a0 < a1
;;;     b0 < b1
;;;     1 = a0 + b0
;;;     m = a0.b0
;;;     a1 = m + a0
;;;     b1 = m + b0
;;;
;;;
;;; 2 + 2 with a few extra relations

(in-package :user)

(setq *lat*
  (let ((gens '(a0 a1 b0 b1 m 1))
        (uc '(
              (a0 (a1))
              (b0 (b1))
              (m (a1 b1))
              (a1 (1))
              (b1 (1))
              (1 ())
             ))
        (joins '(
                 (a1 ((a0 m)))
                 (b1 ((b0 m)))
                 (1 ((a0 b0)  ))
              ))
        (meets '(
                 (m ((a1 b1)) )
             )) )
    (make-fp-lattice gens uc joins meets t) )
)




