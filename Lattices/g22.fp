;;; g22.fp

;;; G_{22} from the big paper with 0 < p < x and y < q < 1 added.
;;; See Figure 15 of the paper.
;;; 

(in-package :user)

(setq *lat*
  (let ((gens '(a b c d e f x y p q 0 1))
        (uc '(
              (0 (p f ))
	      (p (x q))
	      (f (e d))
	      (d (y))
	      (e (y b))
	      (b (c))
	      (x (a b))
	      (c (1))
	      (a (c))
	      (y (q))
	      (q (1))
	      (1 ())

             ))
        (joins '(
                 (1 ((x d)  ))
                 (c ((a f)  ))
                 (b ((x f)  ))
                 (y ((e d)  ))
                 (q ((p y)  ))
              ))
        (meets '(
                 (0 ( (a y)  ))
                 (x ( (a b)  ))
                 (e ( (y c)  ))
                 (f ( (c d)  ))
                 (p ((q x)  ))

             )) )
    (make-fp-lattice gens uc joins meets t) )
)




