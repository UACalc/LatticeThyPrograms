;;; fin4.fp	5/20/97

;;; This is the same as fin.fp except we add b \meet c = e.


;;; All ji's in  the join closure of the meet closure of P are cji.
;;; But b \j (a \m c) is not cmi and it is the only one in 
;;; the meet closure of the join closure of P.

;;;        a o    
;;;          | b c
;;;          | o o
;;;          |/ \|
;;;        d o   o e




(in-package :user)

(setq *lat*
  (let ((gens '(a b c d e))
        (uc '(
              (d (a b))
              (e (b c))
              (a ())
              (b ())
              (c ())
             ))
        (joins '(
              (b ((d e)) )
              ))
        (meets '(
              (e ((b c)) )
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


