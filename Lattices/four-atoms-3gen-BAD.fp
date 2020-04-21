; file atomless.fp


;      a   b   c
;      o   o   o
;       \  |  /
;        \ | /
;         \|/
;          o z

; with all three meets defined

(in-package :user)

(let* ((gens '(zero x y z a b c d x* y* z* x0 y0 z0 ))
       (uc '(
           (x* () )
           (y* () )
           (z* () )
           (x (x*) )
           (y (y*) )
           (z (z*) )
           (x0 (x d))
           (y0 (y d))
           (z0 (z d))
           (d (x* y* z*))
           (a (y z d))
           (b (x z d))
           (c (x y d))
	   (zero (a b c))))
       (joins '(
              (x* ((x a)))
              (y* ((y b)))
              (z* ((z c)))
))
       (meets '(
              (a ((y z)))
              (b ((x z)))
              (c ((x y)))
              (d ((x* y* z*)))
              (zero ((x d)))
              (zero ((y d)))
              (zero ((z d)))
              (zero ((x y z))))))
      (setq four-atoms (make-fp-lattice gens uc joins meets t)))

(setq *lat* four-atoms)






