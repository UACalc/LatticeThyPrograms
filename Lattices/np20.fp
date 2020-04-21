; np20.fp   
;;; this lattice is finite but np19 (probably) isn't.


(in-package 'user)

;(load "bldnew")

(setq np20
  (make-fp-lattice
    '(a b c d e 0 1)
    '(
      (d (1))
      (e (1))
      (b (d))
      (c (e))
      (a (d e))
      (0 (a b c)) )
    '( 
      (1 ((b c))) 
      (d ((a b)))	; this is where np20 differs from np19
      (e ((a c))) )
    '(
      (0 ((d c) (e b))) )))


(setq *lat* (add-cover-stuff np20))
(save-lattice 'np20 "np20.sav")
;(quit)

;
;
;			 o 1 
;			/ \
;		       /   \
;		    d o     o e
;		     / \   / \
;		    /   \ /   \
;                b o     o a   o c
;		    \    |    /
;		     \   |   /
;		      \  |  /
;		       \ | /
;			\|/
;			 o  0 	
;
