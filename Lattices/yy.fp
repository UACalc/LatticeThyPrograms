; yy.fp   


(in-package 'user)

;(load "bldnew")

(setq yy
  (make-fp-lattice
    '(a b c z )
    '(
      (a ())
      (b ())
      (c ())
      (z (a b c)) )
    nil
    '( 
      (z ((a b) (a c) (b c))))))


;(setq *lat* (add-cover-stuff xx))
;(save-lattice 'xx "xx.sav")
;(quit)

