; file np41.fp

(in-package 'user)
;(load "bldnew")
(setq gens '(1 a aa b bb w ww)
      uc '(
	   (a (aa))
	   (b (bb))
	   (aa (1))
	   (bb (1))
	   (w (aa bb))
	   (ww (w)) )
      joins '(
	      (1  ((a b)))
	      (aa ((a ww)))
	      (bb ((b ww))) ) )
(setq np41 (make-fp-lattice gens uc joins nil))
(add-cover-stuff np41)
(setq *lat* np41)
;(save-lattice 'np41 "np41.sav")
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
