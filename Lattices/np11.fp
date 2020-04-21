; np11.fp   


(in-package 'user)

(load "bldnew")

(setq np11
  (make-fp-lattice
    '(a v1 v2 u1 u2 t1 t2 f e1 e2 g)
    '(
      (a (t1 t2 f))
      (e1 (f))
      (e2 (f))
      (g (a e1 e2))
      (u1 (t1))
      (u2 (t2))
      (v1 (a u2))
      (v2 (a u1)) )
    '( 
      (t1 ((v1 u1)))
      (t2 ((v2 u2)))
      (f ((e1 e2))) 
      (f ((a e1))) 
      (f ((a e2))) )
    '(
      (g ((e1 e2) (a e1) (a e2)))
      (v1 ((a u2) ))
      (v2 ((a u1) )) )))


(setq *lat* (add-cover-stuff np11))
(save-lattice 'np11 "np11.sav")
(quit)

