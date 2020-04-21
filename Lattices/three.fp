; three.fp		4/29/97

;;; this shows that if all pairwise joins are 1 and all but one meet is 0
;;; is infinite.

(in-package :user)


(setq *lat*
  (let ((gens '(0 1 a b c ))
        (uc '(
              (0 (a b c))
              (a (1))
              (b (1))
              (c (1))
              (1 ())
           ))
        (meets '(
              ;; (0 ((a c)))
              (0 ((a c) (a b) (b c)))
              ))
        (joins '(
              (1 ((a b) (b c)))
             )))
    (make-fp-lattice gens uc joins meets t ) ) )


