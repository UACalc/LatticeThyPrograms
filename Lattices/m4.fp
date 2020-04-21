; m4.fp  
; this is just m4



(setq m4
  (make-fp-lattice
    '(a b c d e z)
    '(
      (z (a b c d e))
      (a (e))
      (b (e))
      (c (e))
      (d (e)) )
    '( 
      (e ((c d) (a b) (a c) (a d) (b c) (b d)) ) )
    '(
      (z ((c d) (a b) (a c) (a d) (b c) (b d)) ) ) ) )


;(setq *lat* (add-cover-stuff xx))
;(save-lattice 'xx "xx.sav")
;(quit)

