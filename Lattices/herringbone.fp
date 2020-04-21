
; herringbone.fp

;;; This has 3 elements a, b, c forming an antichain, and the meet of
;;; no pair of them defined. It starts a herringbone but it stops.
;;; t_0 = b \meet c,
;;; t_1 = b \meet (a \join (b \meet c)),
;;; t_2 = b \meet (c\join (b \meet (a \join (b \meet c)))),

;;; t_0 < t_1 = t_2. Jezek and Slavik show that if P is join trivial
;;; FL(P) must be infinite under these hypotheses.


(in-package :user)


(setq *lat*
  (let ((gens '(a b c p e aa q q0 q1 q2 q3 q4 q5 q6))
        (uc '(
              (e (b c q q2 q6))
              (aa (a p))
              (a (q3))
              (b (q0 q4 q5))
              (c (q1))
              (p (q3))
              (q (q0 q1 p))
              (q0 ())
              (q1 ())
              (q2 (q1 q3 q4))
              (q3 ())
              (q4 ())
              (q5 ())
              (q6 (c q5))
           ))
        (meets '(
              (q ((q0 q1)))
              (q2 ((q3 q4)))
              (q6 ((q5 c)))
             ))
        (joins '(
              (p ((aa e)))
             )))
    (make-fp-lattice gens uc joins meets t ) )
  herringbone *lat*)

