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
  (let ((gens '(a b m c))
        (uc '(
              (a (c))
              (b (c))
              (m (c))
              (c ())   ;; c is 1
             ))
        (joins '(
                 (c ((a b)  ))
              ))
        (meets '(
             )) )
    (make-fp-lattice gens uc joins meets t) )
)



