; n33plustwo.fp
;;; this is the lattice b4 with one additional element 0 < p < a.

;
;             o
;            /|\
;           / | \
;          /  |  \
;         o   o   o
;         |\ / \ /|
;         | x   x |		change this picture to b4
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


(setq *lat*
  (let ((gens '(0 1 a0 a1 a2 b0 b1 b2 p q ))
        (uc '(
              (0 (p b0 ))
              (p (a0 q))
              (a0 (a1))
              (a1 (a2))
              (a2 (1))
              (b0 (b1))
              (b1 (b2))
              (b2 (q))
              (q (1))
              (1 ())
           ))
        (meets '(
              (0 ((a2 b2) ))
              (p ((a0 q))) 
             ))
        (joins '(
              (1 ((a0 b0)))
              (q ((b2 p)))
             )))
    (make-fp-lattice gens uc joins meets nil ) ) )


