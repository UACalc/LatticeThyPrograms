; np2.fp   

(setq np2
  (make-fp-lattice
    '(0 a b c 1)
    '(
      (0 (a b c))
      (a (1))
      (b (1))
      (c (1)) )
    '(
      (1 ((c b) )) )  
    '(
      (0 ((a b) (a c))))))

(setq *lat* np2)
