; np12.fp   


(in-package 'user)

(load "bldnew")

(setq np12
  (make-fp-lattice
    '(x a b c 0 1)
    '(
      (x (1))
      (b (1))
      (c (1))
      (a (x))
      (0 (a b c)) )
    '( 
      (1 ((a b) (a c) (b c))) )
    '(
      (0 ((a b) (a c) (b c))) ) ))


(setq *lat* (add-cover-stuff np12))
(save-lattice 'np12 "np12.sav")
;(quit)

