; np6.fp   2/11/89

(in-package 'user)

(load "bldnew")

(setq np6
  (make-fp-lattice
    '(r1 s1 u1 v1 r2 s2 u2 v2 a 1 0)
    '(
      (u1 (1))
      (u2 (1))
      (0 (r1 v1 r2 v2))
      (s1 (1))
      (s2 (1))
      (r1 (u1 a))
      (r2 (u2 a))
      (a (s1 s2))
      (v1 (s1)) 
      (v2 (s2)) )
    '(
      (1 ((u1 v1) (u2 v2) ))
      (s1 ((r1 v1) ))
      (s2 ((r2 v2) )) )
    '(
      (0 ((u1 v1) (u2 v2)))
      (r1 ((s1 u1)))
      (r2 ((s2 u2))))))


(setq *lat* (add-cover-stuff np6))
(save-lattice 'np6 "np6.sav")

