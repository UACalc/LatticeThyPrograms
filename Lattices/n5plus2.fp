; n5plus2.fp
;;; this is 3 x 2 glued to N_5 over a three element chain.

(in-package :user)


(setq *lat*
  (let ((gens '(0 1 a b c d e f p  ))
        (uc '(
	      (p (b))
              (0 (d f))
              ;;(p (d))
              ;;(p (d q))
              ;;(f (c b))
              (f (p))
              (d (c e))
              (e (a))
              (a (1))
              (b (1))
	      (c (a))
              (1 ())
           ))
        (meets '(
              (0 ((e b) ))
              (f ((a b) ))
              (d ((c e) ))
             ))
        (joins '(
              (1 ((b d)))
              ;;(1 ((b p)))
              ;;(a ((e f)))
              (a ((e f) ))
              (c ((d f)))
             )))
    (make-fp-lattice gens uc joins meets t ) ) )


