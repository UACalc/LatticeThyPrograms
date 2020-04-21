; p24.fp   

(setq p24
  (make-fp-lattice
    '(z a b c d e)
    '(
      (z (a b))
      (a (c))
      (b (d))
      (c (e))
      (d (e))
      (e ()) )
    '(
      (e ((c b) )) )
    '(
      (z ((c b) (a d))) ) ))


(setq *lat* p24)
