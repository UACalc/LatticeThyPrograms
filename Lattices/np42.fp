; file np42.fp

(in-package 'user)
(load "bldnew")
(setq gens '(1 a e0 e1 e2 z11 z12 z13 z)
      uc '(
	   (a (1))
	   (z11 (e1))
	   (z12 (e1))
	   (z13 (e1))
           (z (z11 z12 z13))
	   (e0 (1))
	   (e1 (a))
	   (e2 (a)) )
      joins '(
	      (e1 ((z11 z12) (z11 z13) (z12 z13)))
	      (1  ((e0 e1 e2))) ) 
      meets '(
	      (z ((z11 z12) (z11 z13) (z12 z13)))))
(setq np42 (make-fp-lattice gens uc joins nil t))
;(add-cover-stuff np42)
(setq *lat* np42)
(save-lattice 'np42 "np42.sav")
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
