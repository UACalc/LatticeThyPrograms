; np16.fp   


(in-package 'user)

;(load "bldnew")

(setq np16
  (make-fp-lattice
    '(x a b c 0 1)
    '(
      (x (a))
      (b (1))
      (c (1))
      (a (c))
      (0 (x b)) )
    '( 
      (1 ((a b))) )
    '(
      (0 ((b c))) )))


(setq *lat* (add-cover-stuff np16))
(save-lattice 'np16 "np16.sav")
;(quit)

