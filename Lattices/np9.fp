; np9.fp   


(in-package 'user)

;(load "bldnew")

(setq np9
  (make-fp-lattice
    '(a v1 v2 u1 u2 t1 t2 f e1 e2)
    '(
      (a (t1 t2 f))
      (e1 (f))
      (e2 (f))
      (u1 (t1))
      (u2 (t2))
      (v1 (a u2))
      (v2 (a u1)) )
    '( 
      (t1 ((v1 u1)))
      (t2 ((v2 u2)))
      (f ((e1 e2))) )
    '(
      (v1 ((a u2) ))
      (v2 ((a u1) )) )))


(setq *lat* (add-cover-stuff np9))
(save-lattice 'np9 "np9.sav")
;(quit)

