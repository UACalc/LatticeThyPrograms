; file lower-atomic.fp

(in-package 'user)
(setq gens '(p a b c d e)
      uc '(
	   (e (c d))
	   (a (c))
	   (b (d))
	   (c (p))
	   (d (p)))
      joins '(
	      (p ((a d) (b c) (a b e))))
      meet nil )
      ;meets '(
;	      (e ((c d)))))
(setq la (make-fp-lattice gens uc joins meets nil))
(add-cover-stuff la)
(setq *lat* la)
;(save-lattice 'la "la.sav")
;(quit)


;
;                        o p = a+d = b+d
;                       / \
;                      /   \
;                   c o     o d
;                    /       \
;                   /         \
;                  /           \
;               a o             o b
