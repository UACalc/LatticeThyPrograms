;;; fin5.fp	5/21/97

;;; This is the same as fin.fp except we add b \meet c = e.
;;; To make b + ac cmi I added f and g and relations f > a and b
;;; g > c and some join and meet rels but it didn't work; see fin6.fp


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
  (let ((gens '(a b c d e f g))
        (uc '(
              (d (a b))
              (e (b c))
              (a (f))
              (b (f))
              (c (f g))
              (f ())
              (g ())
             ))
        (joins '(
              (b ((d e)) )
              (f ((a c)) )
              ))
        (meets '(
              (e ((b c)) )
              (c ((f g)) )
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


