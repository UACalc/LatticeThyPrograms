; d2plusone.fp
;;; this is the lattice d2 the one additional element 0 < p < a.

;
;             o
;            / \
;           /   \
;          /     \
;         o       o
;         |\     /|
;         | \   / |
;         |  \ /  |
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
  (let ((gens '(a b c d e 0 1 p))
        (uc '(
              (0 (p b c))
              (p (a))
              (a (d))
              (b (d e))
              (c (e))
              (d (1))
              (e (1))
              (1 ())
           ))
        (meets '(
              (0 ((d c) (a e)))
              (b ((d e))) ))
        (joins '(
              (1 ((a c)))
              (d ((a b)))
              (e ((c b))) )))
    (make-fp-lattice gens uc joins meets nil ) ) )


