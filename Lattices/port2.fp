; file port2.fp

(in-package 'user)
;(load "bldnew")
(setq gens '(r s p1 p2 q1 q2 z1 z2)
      uc '(
	   (p1 (r s q1))
	   (p2 (r s q2))
	   (z1 (q1 q2))
	   (z2 (q1 q2)) )
      joins '(
	      (q1 ((p1 z1)))
	      (q2 ((p2 z2))) )
      meets nil )
(setq port2 (make-fp-lattice gens uc joins meets nil))
;(add-cover-stuff port2)
(setq *lat* port2)
;(save-lattice 'port2 "port2.sav")
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
