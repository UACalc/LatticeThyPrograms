; b3plusone.fp
;;; this is the lattice b3 with one additional element 0 < p < a.

;
;             o
;            /|\
;           / | \
;          /  |  \
;         o   o   o
;         |\ / \ /|
;         | x   x |
;         |/ \ / \|
;       a o   o   o
;          \  |  /
;         p o | /
;            \|/
;             o
;             a
;
;

(in-package :user)

;; d = a', e = b', f = c'

(setq *lat*
  (let ((gens '(a b c d e f 0 1 p))
        (uc '(
              (0 (p b c))
              (p (a))
              (a (e f))
              (b (d f))
              (c (d e))
              (d (1))
              (e (1))
              (f (1))
              (1 ())
           ))
        (meets '(
              (0 ((d e f) ))
              (a ((e f))) 
              (b ((d f))) 
              (c ((d e))) 
             ))
        (joins '(
              (1 ((a b c)))
              (d ((b c)))
              (e ((a c))) 
              (f ((a b))) 
             )))
    (make-fp-lattice gens uc joins meets nil ) ) )


