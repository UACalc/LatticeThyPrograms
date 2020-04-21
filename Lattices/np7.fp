; np7.fp   2/11/89

(in-package 'user)

(load "bldnew")

(setq np7
  (make-fp-lattice
    '(r1 s1 u1 v1 r2 s2 u2 v2 a e1 e2 0)
    '(
      (u1 (e1))
      (u2 (e2))
      (0 (r1 v1 r2 v2))
      (s1 (e1))
      (s2 (e2))
      (r1 (u1 a))
      (r2 (u2 a))
      (a (s1 s2))
      (v1 (s1)) 
      (v2 (s2)) )
    '(
      (e1 ((u1 v1) ))
      (e2 ((u2 v2) ))
      (s1 ((r1 v1) ))
      (s2 ((r2 v2) )) )
    '(
      (0 ((u1 v1) (u2 v2)))
      (r1 ((s1 u1)))
      (r2 ((s2 u2))))))


(setq *lat* (add-cover-stuff np7))
(save-lattice 'np7 "np7.sav")

