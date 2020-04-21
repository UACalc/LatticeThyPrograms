; np4.fp   2/11/89



(setq np4
  (make-fp-lattice
    '(r s u v a 1)
    '(
      (u (1))
      (s (1))
      (r (u a))
      (a (s))
      (v (s)) )
    '(
      (1 ((u v) ))
      (s ((r v) )) )
    '(
      (r ((s u))))))


(setq *lat* np4)
