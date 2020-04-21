; file port1.fp

(in-package 'user)
;(load "bldnew")
(setq gens '(r s p q2 q3 z1 z2 z3)
      uc '(
	   (p (r s q2 q3))
	   (z1 (q2 q3))
	   (z2 (q2))
	   (z3 (q3)) )
      joins '(
	      (q2 ((p z2)))
	      (q3 ((p z3))) )
      meets nil )
(setq port1 (make-fp-lattice gens uc joins meets nil))
;(add-cover-stuff port1)
(setq *lat* port1)
;(save-lattice 'port1 "port1.sav")
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
