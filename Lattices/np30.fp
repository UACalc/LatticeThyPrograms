; np30.fp   
;;; e is cji and this lattice appears to be infinite (its closure under 's 
;;; and 'p had 96 elements.


;(in-package 'user)

;(load "bldnew")

(setq gens '(z t e e* s r p q u v)
      uc '(
	   (z (r s))
	   (r (e* v))
	   (s (e* u))
	   (e* (e))
	   (e (p q))
	   (v (q))
	   (u (p))
	   (p (t))
	   (q (t)) )
      joins '(
	      (p ( (r u)))
	      (q ( (s v)))
	      (t ( (u v))) ) )
	      
(setq np30 (make-fp-lattice gens uc joins nil))

(add-cover-stuff np30)
(setq *lat* np30)
;(save-lattice 'np30 "np30.sav")

;(quit)

;
;
;			 o t = u+v
;			/ \
;		       /   \
;      	      v+s = q o     o p = u+r
;		     / \   / \
;		    /   \ /   \
;                  /     o e   \
;	        v o      |      o u
;	           \     |     /
;		    \    o e* /
;		     \  / \  /
;		    r o     o s 
;		       \   /
;		        \ /
;			 o
