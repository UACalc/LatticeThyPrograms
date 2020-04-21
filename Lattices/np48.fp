; file np48.fp

(in-package 'user)
;(load "bldnew")
(setq gens '(1 a e0 e1 e2 )
      uc '(
	   (a (1))
	   (e0 (1))
	   (e1 (a))
	   (e2 (1)) )
      joins '(
	      (1  ((e0 e1 e2))) ) 
      meets nil )
(setq np48 (make-fp-lattice gens uc joins nil t))
;(add-cover-stuff np48)
(setq *lat* np48)
;(save-lattice 'np48 "np48.sav")
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
