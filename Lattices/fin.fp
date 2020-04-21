;;; fin.fp	5/20/97

;;; This is part of an investigation of when a(b+c) is cji.
;;; Under rather mild assumptions it isn't; but in this case it is.

;;; The only element of the join closure of the meet closure of P
;;; which is ji but not cji is  a \meet b \mmet c.

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
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


