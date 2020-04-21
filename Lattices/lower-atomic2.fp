; file lower-atomic2.fp

(in-package 'user)
(setq gens '(p a b c d e f)
      uc '(
	   (f (p))
	   (e (p))
	   (d (p))
	   (a (d))
	   (b (d e))
	   (c (e)))
      joins '(
	      (p ((a b c) (a f) (c f) )))
      meets nil )
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
;                   d o     o e
;                    /\     /\
;                   /  \   /  \
;                  /    \ /    \
;               a o      o b    o c
