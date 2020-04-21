; np13.fp   


(in-package 'user)

(load "bldnew")

(setq np13
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
      (0 ((x b) (x c) (a b) (a c) (b c))) ) ))


(setq *lat* (add-cover-stuff np13))
(save-lattice 'np13 "np13.sav")
;(quit)

