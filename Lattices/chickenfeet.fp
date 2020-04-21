; file chickenfeet.fp


;      a   b   c
;      o   o   o
;       \  |  /
;        \ | /
;         \|/
;          o z

; with all three meets defined

(in-package :user)

(let* ((gens '(a b c z))
       (uc '(
           (a ())
           (b ())
           (c ())
	   (z (a b c))))
       (joins '())
       (meets '(
	      (z ((a b) (a c) (b c))))))
      (setq chickenfeet (make-fp-lattice gens uc joins meets t)))

(setq *lat* chickenfeet)


;
;                        o 1 = u+v
;                       / \
;                      /   \
;             v+s = q o     o p = u+r
;                    / \   / \
;                   /   \ /   \
;                  /     o e   \
;               v o      |      o u
;                  \     |     /
;                   \    o e* /
;                    \  / \  /
;                   r o     o s
;                      \   /
;                       \ /
;                        o
