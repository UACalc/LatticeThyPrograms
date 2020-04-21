; file scratch.fp

(in-package 'user)
(setq gens '(a b c d )
      uc '(
	   (d (a b c)))
      meets '(
	      (d ((a c) (b c) )))
      joins nil )
      ;meets '(
;	      (e ((c d)))))
(setq scratch (make-fp-lattice gens uc joins meets t))
(setq *lat* scratch)
;(save-lattice 'la "la.sav")
;(quit)


