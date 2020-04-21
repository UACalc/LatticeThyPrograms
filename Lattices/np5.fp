; np5.fp   2/11/89

(in-package 'user)

(setq np5
  (make-fp-lattice
    '(r s u v a 1 0)
    '(
      (u (1))
      (0 (r v))
      (s (1))
      (r (u a))
      (a (s))
      (v (s)) )
    '(
      (1 ((u v) ))
      (s ((r v) )) )
    '(
      (0 ((u v)))
      (r ((s u))))))


(setq *lat* (add-cover-stuff np5))

