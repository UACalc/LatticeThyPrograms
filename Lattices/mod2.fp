; mod2.fp   


(in-package 'user)

;(load "bldnew")

(setq mod2
  (make-fp-lattice
    '(a b c d f g)
    '(
      (a (f))
      (b (f))
      (c (f g))
      (d (g)) )
    '( 
      (f ((a b) (a c) (b c)))
      (g ((c d) )) )
     nil ))



(setq mod2a
  (make-fp-lattice
    '(a b c d f )
    '(
      (a (f))
      (b (f))
      (c (f )) )
    '( 
      (f ((a b) (a c) (b c))) )
     nil ))


