; file np62.fp

(in-package 'user)
;(load "bldnew")
(setq gens '(1 a a1 b b1 u v)
      uc '(
	   (a (a1))
	   (b (b1))
	   (v (u))
	   (u (a1 b1))
	   (a1 (1))
	   (b1 (1)) )
      joins '(
	      (1 ((a b)))
	      (a1 ((a v)))
	      (b1 ((v b))) )
      meets nil )
(setq np62 (make-fp-lattice gens uc joins meets t))
;(add-cover-stuff np62)
(setq *lat* np62)
;(save-lattice 'np62 "np62.sav")
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
