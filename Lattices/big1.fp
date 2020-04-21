; big1.fp
;;; this is 3 x 3, middle doubled, and a flap and one extra element. 

(in-package :user)

                                   1
;                                   o
;                                  / \
;                               i o   o j
;                                / \ / \
;                               / g o   \ 
;                            e o    |    o h
;                               \ f o   /
;                                \ / \ /
;                               c o   o d
;                                / \ /
;                             a o   o b
;                              p o /
;                                 o 
;                                 0


(setq *lat*
  (let ((gens '(0 1 a b c d e f g h i j p ))
        (uc '(
              (0 (p b))
              (p (a))
              (a (c))
              (b (c d))
              (c (e f))
              (d (f h))
              (e (i))
              (f (g))
              (g (i j))
              (h (j))
              (i (1))
              (j (1))
              (1 ())
           ))
        (meets '(
              (0 ((a h) ))
              (b ((e h) ))
              (c ((e j) ))
              (d ((h i) ))
              (g ((i j) ))
             ))
        (joins '(
              (1 ((e h)))
              (j ((a h)))
              (f ((a d)))
              (c ((a b)))
              (i ((d e)))
             )))
    (make-fp-lattice gens uc joins meets t ) ) )


