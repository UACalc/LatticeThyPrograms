; file np61.fp

(in-package 'user)
;(load "bldnew")
(setq gens '(b e d1 d2 z1 z2)
      uc '(
	   (z1 (d1))
	   (z2 (d2))
	   (b (e))
	   (d1 (e))
	   (d2 (e)) )
      joins '(
	      (e  ((z1 d2 b) (z2 d1 b))))
      meets nil )
(setq np61 (make-fp-lattice gens uc joins meets t))
;(add-cover-stuff np61)
(setq *lat* np61)
;(save-lattice 'np61 "np61.sav")
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
