; file np50.fp

(in-package 'user)
;(load "bldnew")
(setq gens '(e a e0 e1 e2 z1 u1 b)
      uc '(
	   (a (e))
	   (z1 (u1))
	   (b (e1 e2))
	   (e0 (e))
	   (e1 (a u1))
	   (e2 (a)) )
      joins '(
	      (u1 ((z1 b) ))
	      (e  ((e0 e1 e2))) ) 
      meets nil )
(setq np50 (make-fp-lattice gens uc joins meets t))
;(add-cover-stuff np50)
(setq *lat* np50)
;(save-lattice 'np50 "np50.sav")
;(quit)


;
;                        o 1 = u+v
;                       / \
;                      /   \
;             v+s = q o     o p = u+r
;                    / \   / \
;                   /   \ /   \
;                  /     o e   \
;               v o      |      o u
;                  \     |     /
;                   \    o e* /
;                    \  / \  /
;                   r o     o s
;                      \   /
;                       \ /
;                        o
