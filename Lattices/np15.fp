; np15.fp   


(in-package 'user)

(load "bldnew")

(setq np15
  (make-fp-lattice
    '(x a b c d e f 0 1)
    '(
      (x (a))
      (b (d f))
      (c (e f))
      (a (d e))
      (d (1))
      (e (1))
      (f (1))
      (0 (x b c)) )
    '( 
      (1 ((a b c)))
      (d ((a b)))
      (e ((a c)))
      (f ((b c))) )
    '(
      (0 ((a b c)))
      (a ((d e)))
      (b ((d f)))
      (c ((e f))) )))


(setq *lat* (add-cover-stuff np15))
(save-lattice 'np15 "np15.sav")
;(quit)

