; xx.fp   


(in-package 'user)

;(load "bldnew")

(setq xx
  (make-fp-lattice
    '(a b c d e x)
    '(
      (a (x e))
      (b (x))
      (c (e))
      (d (e)) )
    '( 
      (x ((a b)))
      (e ((c d))) ) 
     nil ))


;(setq *lat* (add-cover-stuff xx))
;(save-lattice 'xx "xx.sav")
;(quit)

