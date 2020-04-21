; np14.fp   


(in-package 'user)

(load "bldnew")

(setq np14
  (make-fp-lattice
    '(x a b c 0 1)
    '(
      (x (a))
      (b (1))
      (c (1))
      (a (1))
      (0 (x b c)) )
    '( 
      (1 ((a b) (a c) (b c))) )
    '(
      (0 ((a b) (a c) (b c))) ) ))


(setq *lat* (add-cover-stuff np14))
(save-lattice 'np14 "np14.sav")
;(quit)

