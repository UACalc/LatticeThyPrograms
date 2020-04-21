; np17.fp   


(in-package 'user)

;(load "bldnew")

(setq np17
  (make-fp-lattice
    '(x a b 0 1)
    '(
      (x (1))
      (b (1))
      (a (1))
      (0 (x a b)) )
    '( 
      (1 ((a b))) )
    '(
      (0 ((a b))) )))


(setq *lat* (add-cover-stuff np17))
(save-lattice 'np17 "np17.sav")
;(quit)

