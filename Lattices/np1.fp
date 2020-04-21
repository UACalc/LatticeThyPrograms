; np1.fp   

(setq np1
  (make-fp-lattice
    '(a b c 1)
    '(
      (a (1))
      (b (1))
      (c (1)) )
    '(
      (1 ((c b) )) )  
   nil))


(setq *lat* np1)
