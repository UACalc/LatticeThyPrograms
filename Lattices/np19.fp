; np19.fp   
;;; this lattice is infinite (probably) but np19 is finite.


(in-package 'user)

;(load "bldnew")

(setq np19
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
      (1 ((b c))) )
    '(
      (0 ((d c) (e b))) )))


(setq *lat* (add-cover-stuff np19))
(save-lattice 'np19 "np19.sav")
;(quit)


;
;
;
;			 o 1 
;			/ \
;		       /   \
;		    d o     o e
;		     / \   / \
;		    /   \ /   \
;                 b o     o a   o c
;		    \    |    /
;		     \   |   /
;		      \  |  /
;		       \ | /
;			\|/
;			 o  0 	
;
