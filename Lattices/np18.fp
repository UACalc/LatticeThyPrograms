; np18.fp   


(in-package 'user)

(load "bldnew")

(setq np18
  (make-fp-lattice
    '(a b c d e f g h 0 1)
    '(
      (d (1))
      (e (1))
      (b (d))
      (c (e))
      (f (a))
      (a (d e))
      (g (f b))
      (h (f c))
      (0 (g h)) )
    '( 
      (1 ((b c))) 
      (d ((h b)))
      (e ((g c)))
      (f ((g h))) )
    '(
      (0 ((b c))) 
      (a ((d e)))
      (g ((e b)))
      (h ((f c))) )))


(setq *lat* (add-cover-stuff np18))
(save-lattice 'np18 "np18.sav")
(quit)

