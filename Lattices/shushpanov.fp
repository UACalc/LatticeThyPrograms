; shushpanov.fp
; 12/31/19 Not right!!! See shushpanov1.fp
;;; this is the free lattice gen by a, b, c with a both
;;; dist and dually dist, b and c both dist.

;
;             o
;            / \
;           /   \
;          /     \
;         o       o
;         |\     /|
;         | \   / |      not yet draw; ignore this
;         |  \ /  |
;       a o   o   o
;          \  |  /
;         p o | /
;            \|/
;             o
;             a
;
;

;(in-package :user)

(setq *lat*
  (let ((gens '(a b c d e u v w a0 a1 b1 c1))
        (uc '(
              (a (a1))
              (b (b1))
              (c (c1))
              (d (a0 b))
              (e (a0 c))
              (u ())
              (v ())
              (w ())
              (a0 (a))
              (a1 (u v))
              (b1 (u w))
              (c1 (v w))
           ))
        (meets '(
              (d ( (a b)))
              (e ( (a c)))
              (a1 ((u v))) 
              (b1 ((u w))) 
              (c1 ((v w))) 
           ))
        (joins '(
              (a0 ((d e)))
              (u ((a b)))
              (v ((a c)))
              (w ((b c)))
           )))
    (make-fp-lattice gens uc joins meets nil ) ) )


