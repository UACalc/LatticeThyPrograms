; np3.fp   

(setq np3
  (make-fp-lattice
    '(0 a b c 1)
    '(
      (0 (a b c))
      (a (1))
      (b (1))
      (c (1)) )
    '(
      (1 ((c b) (a b) (a c) )) )  
    '(
      (0 ((a b) (a c))))))

(setq *lat* np3)
