; mod1.fp   


(in-package 'user)

;(load "bldnew")

(setq mod1
  (make-fp-lattice
    '(a b c d e f g)
    '(
      (a (f))
      (b (f))
      (c (f g))
      (d (g))
      (e (g)) )
    '( 
      (f ((a b) (a c) (b c)))
      (g ((c d) (c e) (d e))))
     nil ))


;(setq *lat* (add-cover-stuff mod1))
;(save-lattice 'mod1 "mod1.sav")
;(quit)

