; file np60.fp

(in-package 'user)
;(load "bldnew")
(setq gens '(e d1 d2 z1 z2)
      uc '(
	   (z1 (d1))
	   (z2 (d2))
	   (d1 (e))
	   (d2 (e)) )
      joins '(
	      (e  ((z1 d2) (z2 d1))))
      meets nil )
(setq np60 (make-fp-lattice gens uc joins meets t))
;(add-cover-stuff np60)
(setq *lat* np60)
;(save-lattice 'np60 "np60.sav")
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
