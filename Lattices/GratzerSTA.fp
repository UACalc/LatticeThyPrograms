; this seems to be stupid

;Acutally the dual of this, labelled differently.
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
;           \ op/
;            \|/
;             o
;             a
;
;

(in-package :user)

(setq *lat*
  (let ((gens '(a0 a1 b0 b1 c t z))
        (uc '(
              (z (a0 b0))
              (a0 (a1 c))
              (b0 (b1 c))
              (a1 (t))
              (b1 (t))
              (c (t))
              (t ())
           ))
        (meets '(
              (a0 ((a1 c) ))
              (b0 ((b1 c) ))
              (z ((a1 b1) ))
               )
        )
        (joins '(
              (t ((a0 b1) (a1 b0)))
             ))
       )
    (make-fp-lattice gens uc joins meets t ) ) )


