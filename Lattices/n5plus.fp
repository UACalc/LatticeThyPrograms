; n5plus.fp
;;; this is 3 x 2 glued to N_5 over a three element chain.

(in-package :user)


(setq *lat*
  (let ((gens '(0 1 a b c d e f p q r ))
        (uc '(
              (0 (p f))
              ;;(p (d))
              ;;(p (d q))
              (p (d r))
              ;;(f (c b))
              (f (c b r))
              (d (c e))
              (e (a))
              (c (a))
              (a (1))
              ;;(b (1))
              (b (q))
              (q (1))
              (1 ())
              (r (q a))
           ))
        (meets '(
              (0 ((e b) ))
              (f ((a b) ))
              (d ((c e) ))
              (p ((d q) ))
              (r ((a q) ))
             ))
        (joins '(
              (1 ((b d)))
              ;;(1 ((b p)))
              ;;(a ((e f)))
              (a ((e f) (r c)))
              (c ((d f)))
              (q ((b p)))
             )))
    (make-fp-lattice gens uc joins meets t ) ) )


