; np31.fp   
;;; e is cji and this lattice appears to be infinite (its closure under 's 
;;; and 'p had 96 elements.
;;  On this one I removed e*.


(in-package 'user)

(load "bldnew")

(setq gens '(0 1 e s r p q u v)
      uc '(
	   (0 (r s))
	   (r (e v))
	   (s (e u))
	   (e (p q))
	   (v (q))
	   (u (p))
	   (p (1))
	   (q (1)) )
      joins '(
	      (p ( (r u)))
	      (q ( (s v)))
	      (1 ( (u v))) ) )
	      
(setq np31 (make-fp-lattice gens uc joins nil))

(add-cover-stuff np31)
(setq *lat* np31)
(save-lattice 'np31 "np31.sav")

(quit)

;
;
;			 o 1 = u+v
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
