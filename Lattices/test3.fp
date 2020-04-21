
;;; this is the dual of test2.fp

;         a1  a2  b1      b2
;         o   o   o       o
;          \  |  / \     /     c
;           \ | /   \   /      o
;            \|/     \ /
;             o       o
;             a       b
;
;
;  a(c + ab) is cji
;  a1(c + ab) is cji
;  b1(c + ab) is cji

(in-package :user)

(setq *lat*
  (let ((gens '(a1 a2 a b1 b2 b c))
        (uc '(
              (a1 ())
              (a2 ())
              (b1 ())
              (b2 ())
              (a (a1 a2 b1))
              (b (b1 b2))
              (c ())
           ))
        (meets '(
              (a ((a1 a2))) 
              (b ((b1 b2))) ))
        (joins nil))
    (make-fp-lattice gens uc joins meets t ) ) )


