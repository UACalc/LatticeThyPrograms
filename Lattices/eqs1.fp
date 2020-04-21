; file eqs1.fp  10/7/91  solving equations

(in-package 'user)
(setq gens '(a b c d e f)
      uc '(
	   (d (a b c e f))
	   (c (f))
	   (e (a b f)))
      meets '(
	      (d ((a c) (b c) ))
	      (e ((a b) (f b))))
      joins '(
	      (f ((e c)))) )
      ;meets '(
;	      (e ((c d)))))
(setq eqs1 (make-fp-lattice gens uc joins meets t))
(setq *lat* eqs1)
;(save-lattice 'la "la.sav")
;(quit)


